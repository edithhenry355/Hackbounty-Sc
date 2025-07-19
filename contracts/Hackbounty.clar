(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_EVENT_NOT_ACTIVE (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_REGISTRATION_CLOSED (err u106))
(define-constant ERR_JUDGING_NOT_STARTED (err u107))
(define-constant ERR_ALREADY_JUDGED (err u108))
(define-constant ERR_NOT_PARTICIPANT (err u109))
(define-constant ERR_INVALID_RANK (err u110))
(define-constant ERR_ACHIEVEMENT_EXISTS (err u111))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u112))
(define-constant ERR_INVALID_ACHIEVEMENT (err u113))

(define-data-var next-event-id uint u1)
(define-data-var next-project-id uint u1)
(define-data-var next-achievement-id uint u1)

(define-map participant-reputation
  principal
  {
    total-score: uint,
    events-participated: uint,
    events-won: uint,
    total-prize-earned: uint,
    reputation-level: uint,
    last-updated: uint
  }
)

(define-map achievement-definitions
  uint
  {
    name: (string-ascii 64),
    description: (string-ascii 256),
    requirement-type: (string-ascii 32),
    threshold-value: uint,
    reputation-points: uint,
    is-active: bool
  }
)

(define-map participant-achievements
  {participant: principal, achievement-id: uint}
  {earned-at: uint, event-id: (optional uint)}
)

(define-map reputation-leaderboard
  uint
  {
    participant: principal,
    reputation-score: uint,
    rank: uint
  }
)

(define-map events
  uint
  {
    organizer: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    total-bounty: uint,
    remaining-bounty: uint,
    registration-start: uint,
    registration-end: uint,
    judging-start: uint,
    judging-end: uint,
    winners-announced: bool,
    is-active: bool
  }
)

(define-map projects
  uint
  {
    event-id: uint,
    participant: principal,
    name: (string-ascii 64),
    description: (string-ascii 256),
    repository-url: (string-ascii 128),
    demo-url: (optional (string-ascii 128)),
    submission-block: uint,
    score: uint,
    rank: uint,
    is-winner: bool
  }
)

(define-map event-participants
  {event-id: uint, participant: principal}
  {project-id: uint, registered-at: uint}
)

(define-map judges
  {event-id: uint, judge: principal}
  {authorized: bool, assigned-at: uint}
)

(define-map project-votes
  {project-id: uint, judge: principal}
  {score: uint, voted-at: uint}
)

(define-map event-rankings
  {event-id: uint, rank: uint}
  {project-id: uint, prize-amount: uint, paid: bool}
)

(define-public (create-event 
  (name (string-ascii 64))
  (description (string-ascii 256))
  (total-bounty uint)
  (registration-duration uint)
  (judging-duration uint))
  (let
    (
      (event-id (var-get next-event-id))
      (current-block stacks-block-height)
      (registration-end (+ current-block registration-duration))
      (judging-start (+ registration-end u1))
      (judging-end (+ judging-start judging-duration))
    )
    (asserts! (> total-bounty u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? total-bounty tx-sender (as-contract tx-sender)))
    (map-set events event-id
      {
        organizer: tx-sender,
        name: name,
        description: description,
        total-bounty: total-bounty,
        remaining-bounty: total-bounty,
        registration-start: current-block,
        registration-end: registration-end,
        judging-start: judging-start,
        judging-end: judging-end,
        winners-announced: false,
        is-active: true
      }
    )
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

(define-public (register-project
  (event-id uint)
  (name (string-ascii 64))
  (description (string-ascii 256))
  (repository-url (string-ascii 128))
  (demo-url (optional (string-ascii 128))))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (project-id (var-get next-project-id))
      (current-block stacks-block-height)
    )
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (asserts! (and (>= current-block (get registration-start event))
                   (<= current-block (get registration-end event))) ERR_REGISTRATION_CLOSED)
    (asserts! (is-none (map-get? event-participants {event-id: event-id, participant: tx-sender})) ERR_ALREADY_EXISTS)
    
    (map-set projects project-id
      {
        event-id: event-id,
        participant: tx-sender,
        name: name,
        description: description,
        repository-url: repository-url,
        demo-url: demo-url,
        submission-block: current-block,
        score: u0,
        rank: u0,
        is-winner: false
      }
    )
    (map-set event-participants {event-id: event-id, participant: tx-sender}
      {project-id: project-id, registered-at: current-block}
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (add-judge (event-id uint) (judge-address principal))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get organizer event)) ERR_UNAUTHORIZED)
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (map-set judges {event-id: event-id, judge: judge-address}
      {authorized: true, assigned-at: stacks-block-height}
    )
    (ok true)
  )
)

(define-public (vote-project (project-id uint) (score uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND))
      (event-id (get event-id project))
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (judge-info (unwrap! (map-get? judges {event-id: event-id, judge: tx-sender}) ERR_UNAUTHORIZED))
      (current-block stacks-block-height)
    )
    (asserts! (get authorized judge-info) ERR_UNAUTHORIZED)
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (asserts! (and (>= current-block (get judging-start event))
                   (<= current-block (get judging-end event))) ERR_JUDGING_NOT_STARTED)
    (asserts! (<= score u100) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? project-votes {project-id: project-id, judge: tx-sender})) ERR_ALREADY_JUDGED)
    
    (map-set project-votes {project-id: project-id, judge: tx-sender}
      {score: score, voted-at: current-block}
    )
    (map-set projects project-id
      (merge project {score: (+ (get score project) score)})
    )
    (ok true)
  )
)

(define-public (finalize-rankings (event-id uint) (winner-list (list 10 {project-id: uint, rank: uint})))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get organizer event)) ERR_UNAUTHORIZED)
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (asserts! (> current-block (get judging-end event)) ERR_JUDGING_NOT_STARTED)
    (asserts! (not (get winners-announced event)) ERR_ALREADY_JUDGED)
    
    (try! (process-rankings event-id winner-list))
    (map-set events event-id
      (merge event {winners-announced: true})
    )
    (ok true)
  )
)

(define-private (process-rankings (event-id uint) (winner-list (list 10 {project-id: uint, rank: uint})))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (total-bounty (get total-bounty event))
    )
    (fold process-single-ranking winner-list {event-id: event-id, total-bounty: total-bounty, success: true})
    (ok true)
  )
)

(define-private (process-single-ranking 
  (winner {project-id: uint, rank: uint})
  (acc {event-id: uint, total-bounty: uint, success: bool}))
  (let
    (
      (project-id (get project-id winner))
      (rank (get rank winner))
      (project (unwrap! (map-get? projects project-id) acc))
      (prize-amount (calculate-prize (get total-bounty acc) rank))
    )
    (if (and (get success acc) (<= rank u10) (> prize-amount u0))
      (begin
        (map-set projects project-id
          (merge project {rank: rank, is-winner: true})
        )
        (map-set event-rankings {event-id: (get event-id acc), rank: rank}
          {project-id: project-id, prize-amount: prize-amount, paid: false}
        )
        acc
      )
      (merge acc {success: false})
    )
  )
)

(define-private (calculate-prize (total-bounty uint) (rank uint))
  (if (is-eq rank u1)
    (/ (* total-bounty u50) u100)
    (if (is-eq rank u2)
      (/ (* total-bounty u30) u100)
      (if (is-eq rank u3)
        (/ (* total-bounty u20) u100)
        u0
      )
    )
  )
)

(define-public (claim-prize (event-id uint))
  (let
    (
      (participant-info (unwrap! (map-get? event-participants {event-id: event-id, participant: tx-sender}) ERR_NOT_PARTICIPANT))
      (project-id (get project-id participant-info))
      (project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND))
      (rank (get rank project))
      (ranking-info (unwrap! (map-get? event-rankings {event-id: event-id, rank: rank}) ERR_NOT_FOUND))
      (prize-amount (get prize-amount ranking-info))
    )
    (asserts! (get is-winner project) ERR_NOT_PARTICIPANT)
    (asserts! (not (get paid ranking-info)) ERR_ALREADY_EXISTS)
    (asserts! (> prize-amount u0) ERR_INVALID_AMOUNT)
    
    (try! (as-contract (stx-transfer? prize-amount tx-sender (get participant project))))
    (map-set event-rankings {event-id: event-id, rank: rank}
      (merge ranking-info {paid: true})
    )
    (try! (update-participant-reputation (get participant project) event-id (get score project) prize-amount (get is-winner project)))
    (ok prize-amount)
  )
)

(define-public (emergency-withdraw (event-id uint))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (current-block stacks-block-height)
      (emergency-block (+ (get judging-end event) u1000))
    )
    (asserts! (is-eq tx-sender (get organizer event)) ERR_UNAUTHORIZED)
    (asserts! (> current-block emergency-block) ERR_JUDGING_NOT_STARTED)
    
    (try! (as-contract (stx-transfer? (get remaining-bounty event) tx-sender (get organizer event))))
    (map-set events event-id
      (merge event {is-active: false, remaining-bounty: u0})
    )
    (ok (get remaining-bounty event))
  )
)

(define-read-only (get-event (event-id uint))
  (map-get? events event-id)
)

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-participant-project (event-id uint) (participant principal))
  (map-get? event-participants {event-id: event-id, participant: participant})
)

(define-read-only (get-judge-status (event-id uint) (judge principal))
  (map-get? judges {event-id: event-id, judge: judge})
)

(define-read-only (get-ranking (event-id uint) (rank uint))
  (map-get? event-rankings {event-id: event-id, rank: rank})
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-public (create-achievement
  (name (string-ascii 64))
  (description (string-ascii 256))
  (requirement-type (string-ascii 32))
  (threshold-value uint)
  (reputation-points uint))
  (let
    (
      (achievement-id (var-get next-achievement-id))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> reputation-points u0) ERR_INVALID_AMOUNT)
    (asserts! (> threshold-value u0) ERR_INVALID_AMOUNT)
    
    (map-set achievement-definitions achievement-id
      {
        name: name,
        description: description,
        requirement-type: requirement-type,
        threshold-value: threshold-value,
        reputation-points: reputation-points,
        is-active: true
      }
    )
    (var-set next-achievement-id (+ achievement-id u1))
    (ok achievement-id)
  )
)

(define-public (update-participant-reputation (participant principal) (event-id uint) (score uint) (prize-amount uint) (is-winner bool))
  (let
    (
      (current-rep (default-to 
        {total-score: u0, events-participated: u0, events-won: u0, total-prize-earned: u0, reputation-level: u0, last-updated: u0}
        (map-get? participant-reputation participant)))
      (new-events-participated (+ (get events-participated current-rep) u1))
      (new-events-won (if is-winner (+ (get events-won current-rep) u1) (get events-won current-rep)))
      (new-total-score (+ (get total-score current-rep) score))
      (new-total-prize (+ (get total-prize-earned current-rep) prize-amount))
      (new-reputation-level (calculate-reputation-level new-events-participated new-events-won new-total-score))
    )
    (map-set participant-reputation participant
      {
        total-score: new-total-score,
        events-participated: new-events-participated,
        events-won: new-events-won,
        total-prize-earned: new-total-prize,
        reputation-level: new-reputation-level,
        last-updated: stacks-block-height
      }
    )
    (try! (check-and-award-achievements participant event-id))
    (ok true)
  )
)

(define-private (calculate-reputation-level (events-count uint) (wins uint) (total-score uint))
  (let
    (
      (win-rate (if (> events-count u0) (/ (* wins u100) events-count) u0))
      (avg-score (if (> events-count u0) (/ total-score events-count) u0))
    )
    (if (and (>= events-count u10) (>= win-rate u50))
      u5
      (if (and (>= events-count u7) (>= win-rate u30))
        u4
        (if (and (>= events-count u5) (>= avg-score u70))
          u3
          (if (>= events-count u3)
            u2
            (if (>= events-count u1)
              u1
              u0
            )
          )
        )
      )
    )
  )
)

(define-private (check-and-award-achievements (participant principal) (event-id uint))
  (let
    (
      (reputation (unwrap! (map-get? participant-reputation participant) ERR_NOT_FOUND))
    )
    (if (>= (get events-participated reputation) u1) 
        (unwrap-panic (maybe-award-achievement participant event-id u1)) true)
    (if (>= (get events-participated reputation) u5) 
        (unwrap-panic (maybe-award-achievement participant event-id u2)) true)
    (if (>= (get events-won reputation) u1) 
        (unwrap-panic (maybe-award-achievement participant event-id u3)) true)
    (if (>= (get events-won reputation) u3) 
        (unwrap-panic (maybe-award-achievement participant event-id u4)) true)
    (if (>= (get total-prize-earned reputation) u1000000) 
        (unwrap-panic (maybe-award-achievement participant event-id u5)) true)
    (if (>= (get reputation-level reputation) u5) 
        (unwrap-panic (maybe-award-achievement participant event-id u6)) true)
    (ok true)
  )
)

(define-private (maybe-award-achievement (participant principal) (event-id uint) (achievement-id uint))
  (let
    (
      (achievement (map-get? achievement-definitions achievement-id))
      (already-earned (map-get? participant-achievements {participant: participant, achievement-id: achievement-id}))
    )
    (if (and (is-some achievement) (is-none already-earned))
      (begin
        (map-set participant-achievements {participant: participant, achievement-id: achievement-id}
          {earned-at: stacks-block-height, event-id: (some event-id)}
        )
        (ok true)
      )
      (ok false)
    )
  )
)

(define-public (initialize-default-achievements)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (create-achievement "First Steps" "Participate in your first hackathon" "events_participated" u1 u10))
    (try! (create-achievement "Veteran Hacker" "Participate in 5 hackathons" "events_participated" u5 u50))
    (try! (create-achievement "First Victory" "Win your first hackathon" "events_won" u1 u100))
    (try! (create-achievement "Hat Trick" "Win 3 hackathons" "events_won" u3 u300))
    (try! (create-achievement "Big Earner" "Earn over 1 STX in prizes" "total_prize_earned" u1000000 u200))
    (try! (create-achievement "Reputation Master" "Reach maximum reputation level" "reputation_level" u5 u500))
    (ok true)
  )
)

(define-public (get-reputation-ranking (limit uint))
  (let
    (
      (participants (list))
    )
    (ok participants)
  )
)

(define-read-only (get-participant-reputation (participant principal))
  (map-get? participant-reputation participant)
)

(define-read-only (get-achievement (achievement-id uint))
  (map-get? achievement-definitions achievement-id)
)

(define-read-only (get-participant-achievement (participant principal) (achievement-id uint))
  (map-get? participant-achievements {participant: participant, achievement-id: achievement-id})
)

(define-read-only (calculate-reputation-points (participant principal))
  (let
    (
      (reputation (default-to 
        {total-score: u0, events-participated: u0, events-won: u0, total-prize-earned: u0, reputation-level: u0, last-updated: u0}
        (map-get? participant-reputation participant)))
      (base-points (* (get events-participated reputation) u10))
      (win-bonus (* (get events-won reputation) u50))
      (score-bonus (/ (get total-score reputation) u10))
      (level-bonus (* (get reputation-level reputation) u100))
    )
    (+ base-points (+ win-bonus (+ score-bonus level-bonus)))
  )
)

(define-public (manual-reputation-update (participant principal) (event-id uint))
  (let
    (
      (participant-info (unwrap! (map-get? event-participants {event-id: event-id, participant: participant}) ERR_NOT_PARTICIPANT))
      (project-id (get project-id participant-info))
      (project (unwrap! (map-get? projects project-id) ERR_NOT_FOUND))
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
    )
    (asserts! (get winners-announced event) ERR_JUDGING_NOT_STARTED)
    (try! (update-participant-reputation participant event-id (get score project) u0 (get is-winner project)))
    (ok true)
  )
)

(define-public (toggle-achievement-status (achievement-id uint) (active bool))
  (let
    (
      (achievement (unwrap! (map-get? achievement-definitions achievement-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set achievement-definitions achievement-id
      (merge achievement {is-active: active})
    )
    (ok true)
  )
)

(define-read-only (get-participant-achievements-list (participant principal))
  (let
    (
      (achievements-earned (list))
    )
    (ok achievements-earned)
  )
)

(define-read-only (get-reputation-stats (participant principal))
  (let
    (
      (reputation (map-get? participant-reputation participant))
      (total-points (calculate-reputation-points participant))
    )
    (match reputation
      rep (ok {
        reputation: rep,
        total-reputation-points: total-points,
        achievements-count: u0
      })
      (ok {
        reputation: {total-score: u0, events-participated: u0, events-won: u0, total-prize-earned: u0, reputation-level: u0, last-updated: u0},
        total-reputation-points: u0,
        achievements-count: u0
      })
    )
  )
)
