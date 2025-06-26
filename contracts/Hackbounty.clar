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

(define-data-var next-event-id uint u1)
(define-data-var next-project-id uint u1)

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
