;; Team Formation System
;; Enables participants to form teams, manage members, and submit collaborative hackathon projects

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_TEAM_NOT_FOUND (err u201))
(define-constant ERR_ALREADY_EXISTS (err u202))
(define-constant ERR_TEAM_FULL (err u203))
(define-constant ERR_NOT_TEAM_MEMBER (err u204))
(define-constant ERR_NOT_TEAM_LEADER (err u205))
(define-constant ERR_INVALID_ROLE (err u206))
(define-constant ERR_EVENT_NOT_ACTIVE (err u207))
(define-constant ERR_ALREADY_IN_TEAM (err u208))
(define-constant ERR_INVITATION_NOT_FOUND (err u209))
(define-constant ERR_TEAM_ALREADY_SUBMITTED (err u210))

;; Data variables
(define-data-var next-team-id uint u1)
(define-data-var max-team-size uint u5)
(define-data-var total-teams-created uint u0)

;; Team data structure
(define-map teams
  uint
  {
    event-id: uint,
    team-name: (string-ascii 64),
    leader: principal,
    description: (string-ascii 256),
    member-count: uint,
    max-members: uint,
    created-at: uint,
    is-active: bool,
    project-submitted: bool,
    looking-for-members: bool
  }
)

;; Team membership
(define-map team-members
  {team-id: uint, member: principal}
  {
    role: (string-ascii 32),
    joined-at: uint,
    contribution-score: uint,
    is-active: bool
  }
)

;; Team invitations
(define-map team-invitations
  {team-id: uint, invitee: principal}
  {
    invited-by: principal,
    role-offered: (string-ascii 32),
    invited-at: uint,
    status: (string-ascii 16),
    message: (string-ascii 128)
  }
)

;; Event team tracking
(define-map event-teams
  uint
  {
    teams-count: uint,
    max-team-size: uint,
    team-registration-open: bool
  }
)

;; Participant team status
(define-map participant-teams
  {event-id: uint, participant: principal}
  {
    team-id: uint,
    role: (string-ascii 32),
    is-leader: bool
  }
)

;; Team skills tracking
(define-map team-skills
  uint
  {
    required-skills: (list 5 (string-ascii 32)),
    available-skills: (list 5 (string-ascii 32)),
    skill-gaps: (list 5 (string-ascii 32))
  }
)

;; Initialize team settings for an event
(define-public (initialize-team-system (event-id uint) (max-size uint))
  (let (
    (event (unwrap! (contract-call? .Hackbounty get-event event-id) ERR_EVENT_NOT_ACTIVE))
  )
    (asserts! (is-eq tx-sender (get organizer event)) ERR_UNAUTHORIZED)
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (asserts! (and (> max-size u1) (<= max-size u10)) ERR_INVALID_ROLE)
    
    (map-set event-teams event-id
      {
        teams-count: u0,
        max-team-size: max-size,
        team-registration-open: true
      }
    )
    
    (ok true)
  )
)

;; Create a new team
(define-public (create-team
  (event-id uint)
  (team-name (string-ascii 64))
  (description (string-ascii 256))
  (max-members uint)
  (required-skills (list 5 (string-ascii 32)))
)
  (let (
    (team-id (var-get next-team-id))
    (event (unwrap! (contract-call? .Hackbounty get-event event-id) ERR_EVENT_NOT_ACTIVE))
    (event-settings (default-to {teams-count: u0, max-team-size: u5, team-registration-open: false} 
                                (map-get? event-teams event-id)))
    (current-block stacks-block-height)
    (final-max-members (if (<= max-members (get max-team-size event-settings)) max-members (get max-team-size event-settings)))
  )
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (asserts! (get team-registration-open event-settings) ERR_EVENT_NOT_ACTIVE)
    (asserts! (is-none (map-get? participant-teams {event-id: event-id, participant: tx-sender})) ERR_ALREADY_IN_TEAM)
    (asserts! (and (>= current-block (get registration-start event)) 
                   (<= current-block (get registration-end event))) ERR_EVENT_NOT_ACTIVE)
    
    ;; Create the team
    (map-set teams team-id
      {
        event-id: event-id,
        team-name: team-name,
        leader: tx-sender,
        description: description,
        member-count: u1,
        max-members: final-max-members,
        created-at: current-block,
        is-active: true,
        project-submitted: false,
        looking-for-members: true
      }
    )
    
    ;; Add leader as team member
    (map-set team-members {team-id: team-id, member: tx-sender}
      {
        role: "Leader",
        joined-at: current-block,
        contribution-score: u0,
        is-active: true
      }
    )
    
    ;; Track participant team status
    (map-set participant-teams {event-id: event-id, participant: tx-sender}
      {
        team-id: team-id,
        role: "Leader",
        is-leader: true
      }
    )
    
    ;; Set team skills
    (map-set team-skills team-id
      {
        required-skills: required-skills,
        available-skills: (list),
        skill-gaps: required-skills
      }
    )
    
    ;; Update event statistics
    (map-set event-teams event-id
      (merge event-settings {teams-count: (+ (get teams-count event-settings) u1)})
    )
    
    ;; Update global counters
    (var-set next-team-id (+ team-id u1))
    (var-set total-teams-created (+ (var-get total-teams-created) u1))
    
    (ok team-id)
  )
)

;; Invite a member to join the team
(define-public (invite-member
  (team-id uint)
  (invitee principal)
  (role-offered (string-ascii 32))
  (message (string-ascii 128))
)
  (let (
    (team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND))
    (team-membership (map-get? team-members {team-id: team-id, member: tx-sender}))
    (existing-invitation (map-get? team-invitations {team-id: team-id, invitee: invitee}))
    (invitee-team-status (map-get? participant-teams {event-id: (get event-id team), participant: invitee}))
    (current-block stacks-block-height)
  )
    (asserts! (get is-active team) ERR_TEAM_NOT_FOUND)
    (asserts! (or (is-eq tx-sender (get leader team)) 
                  (is-some team-membership)) ERR_NOT_TEAM_LEADER)
    (asserts! (< (get member-count team) (get max-members team)) ERR_TEAM_FULL)
    (asserts! (is-none existing-invitation) ERR_ALREADY_EXISTS)
    (asserts! (is-none invitee-team-status) ERR_ALREADY_IN_TEAM)
    
    (map-set team-invitations {team-id: team-id, invitee: invitee}
      {
        invited-by: tx-sender,
        role-offered: role-offered,
        invited-at: current-block,
        status: "pending",
        message: message
      }
    )
    
    (ok true)
  )
)

;; Accept a team invitation
(define-public (accept-invitation (team-id uint))
  (let (
    (team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND))
    (invitation (unwrap! (map-get? team-invitations {team-id: team-id, invitee: tx-sender}) ERR_INVITATION_NOT_FOUND))
    (current-block stacks-block-height)
    (new-member-count (+ (get member-count team) u1))
  )
    (asserts! (get is-active team) ERR_TEAM_NOT_FOUND)
    (asserts! (is-eq (get status invitation) "pending") ERR_INVITATION_NOT_FOUND)
    (asserts! (< (get member-count team) (get max-members team)) ERR_TEAM_FULL)
    (asserts! (is-none (map-get? participant-teams {event-id: (get event-id team), participant: tx-sender})) ERR_ALREADY_IN_TEAM)
    
    ;; Add member to team
    (map-set team-members {team-id: team-id, member: tx-sender}
      {
        role: (get role-offered invitation),
        joined-at: current-block,
        contribution-score: u0,
        is-active: true
      }
    )
    
    ;; Update participant status
    (map-set participant-teams {event-id: (get event-id team), participant: tx-sender}
      {
        team-id: team-id,
        role: (get role-offered invitation),
        is-leader: false
      }
    )
    
    ;; Update team member count
    (map-set teams team-id
      (merge team {
        member-count: new-member-count,
        looking-for-members: (< new-member-count (get max-members team))
      })
    )
    
    ;; Update invitation status
    (map-set team-invitations {team-id: team-id, invitee: tx-sender}
      (merge invitation {status: "accepted"})
    )
    
    (ok true)
  )
)

;; Decline a team invitation
(define-public (decline-invitation (team-id uint))
  (let (
    (invitation (unwrap! (map-get? team-invitations {team-id: team-id, invitee: tx-sender}) ERR_INVITATION_NOT_FOUND))
  )
    (asserts! (is-eq (get status invitation) "pending") ERR_INVITATION_NOT_FOUND)
    
    (map-set team-invitations {team-id: team-id, invitee: tx-sender}
      (merge invitation {status: "declined"})
    )
    
    (ok true)
  )
)

;; Leave a team (member only, not leader)
(define-public (leave-team (team-id uint))
  (let (
    (team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND))
    (member-info (unwrap! (map-get? team-members {team-id: team-id, member: tx-sender}) ERR_NOT_TEAM_MEMBER))
    (participant-status (unwrap! (map-get? participant-teams {event-id: (get event-id team), participant: tx-sender}) ERR_NOT_TEAM_MEMBER))
    (new-member-count (- (get member-count team) u1))
  )
    (asserts! (get is-active team) ERR_TEAM_NOT_FOUND)
    (asserts! (get is-active member-info) ERR_NOT_TEAM_MEMBER)
    (asserts! (not (is-eq tx-sender (get leader team))) ERR_NOT_TEAM_LEADER)
    (asserts! (not (get project-submitted team)) ERR_TEAM_ALREADY_SUBMITTED)
    
    ;; Remove member from team
    (map-set team-members {team-id: team-id, member: tx-sender}
      (merge member-info {is-active: false})
    )
    
    ;; Remove participant team status
    (map-delete participant-teams {event-id: (get event-id team), participant: tx-sender})
    
    ;; Update team member count
    (map-set teams team-id
      (merge team {
        member-count: new-member-count,
        looking-for-members: true
      })
    )
    
    (ok true)
  )
)

;; Submit team project (leader only)
(define-public (submit-team-project
  (team-id uint)
  (project-name (string-ascii 64))
  (project-description (string-ascii 256))
  (repository-url (string-ascii 128))
  (demo-url (optional (string-ascii 128)))
)
  (let (
    (team (unwrap! (map-get? teams team-id) ERR_TEAM_NOT_FOUND))
    (event-id (get event-id team))
  )
    (asserts! (get is-active team) ERR_TEAM_NOT_FOUND)
    (asserts! (is-eq tx-sender (get leader team)) ERR_NOT_TEAM_LEADER)
    (asserts! (not (get project-submitted team)) ERR_TEAM_ALREADY_SUBMITTED)
    
    ;; Submit project through main Hackbounty contract
    (let (
      (project-result (try! (contract-call? .Hackbounty register-project
        event-id
        project-name
        project-description
        repository-url
        demo-url
      )))
    )
      ;; Mark team as having submitted
      (map-set teams team-id
        (merge team {project-submitted: true, looking-for-members: false})
      )
      
      (ok project-result)
    )
  )
)

;; Read-only functions

(define-read-only (get-team (team-id uint))
  (map-get? teams team-id)
)

(define-read-only (get-team-member (team-id uint) (member principal))
  (map-get? team-members {team-id: team-id, member: member})
)

(define-read-only (get-team-invitation (team-id uint) (invitee principal))
  (map-get? team-invitations {team-id: team-id, invitee: invitee})
)

(define-read-only (get-participant-team (event-id uint) (participant principal))
  (map-get? participant-teams {event-id: event-id, participant: participant})
)

(define-read-only (get-event-team-settings (event-id uint))
  (map-get? event-teams event-id)
)

(define-read-only (get-team-skills (team-id uint))
  (map-get? team-skills team-id)
)

(define-read-only (get-team-stats (team-id uint))
  (match (map-get? teams team-id)
    team (some {
           member-count: (get member-count team),
           max-members: (get max-members team),
           spots-available: (- (get max-members team) (get member-count team)),
           is-recruiting: (get looking-for-members team),
           project-submitted: (get project-submitted team)
         })
    none
  )
)

(define-read-only (is-team-member (team-id uint) (member principal))
  (match (map-get? team-members {team-id: team-id, member: member})
    member-info (get is-active member-info)
    false
  )
)

(define-read-only (is-team-leader (team-id uint) (member principal))
  (match (map-get? teams team-id)
    team (is-eq member (get leader team))
    false
  )
)

(define-read-only (get-team-formation-stats)
  {
    total-teams-created: (var-get total-teams-created),
    max-team-size: (var-get max-team-size),
    next-team-id: (var-get next-team-id)
  }
)
