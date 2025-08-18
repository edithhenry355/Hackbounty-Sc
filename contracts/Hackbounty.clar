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
(define-constant ERR_SPONSOR_EXISTS (err u114))
(define-constant ERR_INSUFFICIENT_SPONSORSHIP (err u115))
(define-constant ERR_INVALID_TIER (err u116))
(define-constant ERR_SPONSORSHIP_CLOSED (err u117))

(define-data-var next-event-id uint u1)
(define-data-var next-project-id uint u1)
(define-data-var next-achievement-id uint u1)
(define-data-var next-sponsorship-id uint u1)

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

(define-map event-sponsors
  {event-id: uint, sponsor: principal}
  {
    sponsorship-id: uint,
    amount-contributed: uint,
    tier: uint,
    sponsored-at: uint,
    company-name: (optional (string-ascii 64)),
    logo-url: (optional (string-ascii 128)),
    is-active: bool
  }
)

(define-map sponsor-tiers
  uint
  {
    tier-name: (string-ascii 32),
    minimum-amount: uint,
    maximum-sponsors: uint,
    benefits-description: (string-ascii 256),
    priority-level: uint,
    is-active: bool
  }
)

(define-map event-sponsorship-pools
  uint
  {
    total-sponsored: uint,
    sponsors-count: uint,
    tier-distribution: {tier1: uint, tier2: uint, tier3: uint},
    sponsorship-deadline: uint,
    is-open: bool
  }
)

(define-map sponsor-analytics
  principal
  {
    total-sponsored: uint,
    events-sponsored: uint,
    average-tier: uint,
    total-exposure: uint,
    last-sponsored: uint
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

(define-public (create-sponsor-tier
  (tier-name (string-ascii 32))
  (minimum-amount uint)
  (maximum-sponsors uint)
  (benefits-description (string-ascii 256))
  (priority-level uint))
  (let
    (
      (tier-id priority-level)
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> minimum-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> maximum-sponsors u0) ERR_INVALID_AMOUNT)
    (asserts! (<= priority-level u3) ERR_INVALID_TIER)
    
    (map-set sponsor-tiers tier-id
      {
        tier-name: tier-name,
        minimum-amount: minimum-amount,
        maximum-sponsors: maximum-sponsors,
        benefits-description: benefits-description,
        priority-level: priority-level,
        is-active: true
      }
    )
    (ok tier-id)
  )
)

(define-public (initialize-sponsorship-for-event (event-id uint) (sponsorship-deadline uint))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get organizer event)) ERR_UNAUTHORIZED)
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (asserts! (> sponsorship-deadline stacks-block-height) ERR_INVALID_AMOUNT)
    
    (map-set event-sponsorship-pools event-id
      {
        total-sponsored: u0,
        sponsors-count: u0,
        tier-distribution: {tier1: u0, tier2: u0, tier3: u0},
        sponsorship-deadline: sponsorship-deadline,
        is-open: true
      }
    )
    (ok true)
  )
)

(define-public (sponsor-event
  (event-id uint)
  (amount uint)
  (tier uint)
  (company-name (optional (string-ascii 64)))
  (logo-url (optional (string-ascii 128))))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (sponsorship-pool (unwrap! (map-get? event-sponsorship-pools event-id) ERR_NOT_FOUND))
      (tier-info (unwrap! (map-get? sponsor-tiers tier) ERR_INVALID_TIER))
      (sponsorship-id (var-get next-sponsorship-id))
      (existing-sponsor (map-get? event-sponsors {event-id: event-id, sponsor: tx-sender}))
      (current-tier-count (get-tier-count event-id tier))
    )
    (asserts! (get is-active event) ERR_EVENT_NOT_ACTIVE)
    (asserts! (get is-open sponsorship-pool) ERR_SPONSORSHIP_CLOSED)
    (asserts! (get is-active tier-info) ERR_INVALID_TIER)
    (asserts! (<= stacks-block-height (get sponsorship-deadline sponsorship-pool)) ERR_SPONSORSHIP_CLOSED)
    (asserts! (>= amount (get minimum-amount tier-info)) ERR_INSUFFICIENT_SPONSORSHIP)
    (asserts! (< current-tier-count (get maximum-sponsors tier-info)) ERR_INVALID_TIER)
    (asserts! (is-none existing-sponsor) ERR_SPONSOR_EXISTS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set event-sponsors {event-id: event-id, sponsor: tx-sender}
      {
        sponsorship-id: sponsorship-id,
        amount-contributed: amount,
        tier: tier,
        sponsored-at: stacks-block-height,
        company-name: company-name,
        logo-url: logo-url,
        is-active: true
      }
    )
    
    (unwrap-panic (update-sponsorship-pool event-id amount tier))
    (unwrap-panic (update-sponsor-analytics tx-sender amount tier))
    
    (map-set events event-id
      (merge event {total-bounty: (+ (get total-bounty event) amount),
                    remaining-bounty: (+ (get remaining-bounty event) amount)})
    )
    
    (var-set next-sponsorship-id (+ sponsorship-id u1))
    (ok sponsorship-id)
  )
)

(define-private (get-tier-count (event-id uint) (tier uint))
  (let
    (
      (pool (default-to 
        {total-sponsored: u0, sponsors-count: u0, tier-distribution: {tier1: u0, tier2: u0, tier3: u0}, sponsorship-deadline: u0, is-open: false}
        (map-get? event-sponsorship-pools event-id)))
      (tier-dist (get tier-distribution pool))
    )
    (if (is-eq tier u1)
      (get tier1 tier-dist)
      (if (is-eq tier u2)
        (get tier2 tier-dist)
        (if (is-eq tier u3)
          (get tier3 tier-dist)
          u0
        )
      )
    )
  )
)

(define-private (update-sponsorship-pool (event-id uint) (amount uint) (tier uint))
  (let
    (
      (pool (unwrap! (map-get? event-sponsorship-pools event-id) ERR_NOT_FOUND))
      (current-tier-dist (get tier-distribution pool))
      (new-tier-dist 
        (if (is-eq tier u1)
          (merge current-tier-dist {tier1: (+ (get tier1 current-tier-dist) u1)})
          (if (is-eq tier u2)
            (merge current-tier-dist {tier2: (+ (get tier2 current-tier-dist) u1)})
            (if (is-eq tier u3)
              (merge current-tier-dist {tier3: (+ (get tier3 current-tier-dist) u1)})
              current-tier-dist
            )
          )
        )
      )
    )
    (map-set event-sponsorship-pools event-id
      (merge pool {
        total-sponsored: (+ (get total-sponsored pool) amount),
        sponsors-count: (+ (get sponsors-count pool) u1),
        tier-distribution: new-tier-dist
      })
    )
    (ok true)
  )
)

(define-private (update-sponsor-analytics (sponsor principal) (amount uint) (tier uint))
  (let
    (
      (analytics (default-to 
        {total-sponsored: u0, events-sponsored: u0, average-tier: u0, total-exposure: u0, last-sponsored: u0}
        (map-get? sponsor-analytics sponsor)))
      (new-events (+ (get events-sponsored analytics) u1))
      (new-total (+ (get total-sponsored analytics) amount))
      (new-avg-tier (/ (+ (* (get average-tier analytics) (get events-sponsored analytics)) tier) new-events))
    )
    (map-set sponsor-analytics sponsor
      {
        total-sponsored: new-total,
        events-sponsored: new-events,
        average-tier: new-avg-tier,
        total-exposure: (+ (get total-exposure analytics) u1),
        last-sponsored: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (withdraw-sponsorship (event-id uint))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (sponsor-info (unwrap! (map-get? event-sponsors {event-id: event-id, sponsor: tx-sender}) ERR_NOT_FOUND))
      (sponsorship-pool (unwrap! (map-get? event-sponsorship-pools event-id) ERR_NOT_FOUND))
      (amount (get amount-contributed sponsor-info))
    )
    (asserts! (get is-active sponsor-info) ERR_NOT_FOUND)
    (asserts! (get is-open sponsorship-pool) ERR_SPONSORSHIP_CLOSED)
    (asserts! (< stacks-block-height (get registration-start event)) ERR_REGISTRATION_CLOSED)
    
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    (map-set event-sponsors {event-id: event-id, sponsor: tx-sender}
      (merge sponsor-info {is-active: false})
    )
    
    (map-set events event-id
      (merge event {total-bounty: (- (get total-bounty event) amount),
                    remaining-bounty: (- (get remaining-bounty event) amount)})
    )
    
    (ok amount)
  )
)

(define-public (close-sponsorship (event-id uint))
  (let
    (
      (event (unwrap! (map-get? events event-id) ERR_NOT_FOUND))
      (sponsorship-pool (unwrap! (map-get? event-sponsorship-pools event-id) ERR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get organizer event)) ERR_UNAUTHORIZED)
    (asserts! (get is-open sponsorship-pool) ERR_SPONSORSHIP_CLOSED)
    
    (map-set event-sponsorship-pools event-id
      (merge sponsorship-pool {is-open: false})
    )
    (ok true)
  )
)

(define-public (initialize-default-sponsor-tiers)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (create-sponsor-tier "Platinum" u5000000 u2 "Top logo placement, speaking slot, premium booth space" u1))
    (try! (create-sponsor-tier "Gold" u2000000 u5 "Logo on materials, booth space, networking access" u2))
    (try! (create-sponsor-tier "Silver" u500000 u10 "Logo mention, basic booth space, swag distribution" u3))
    (ok true)
  )
)

(define-read-only (get-event-sponsors (event-id uint))
  (map-get? event-sponsorship-pools event-id)
)

(define-read-only (get-sponsor-info (event-id uint) (sponsor principal))
  (map-get? event-sponsors {event-id: event-id, sponsor: sponsor})
)

(define-read-only (get-sponsor-tier (tier uint))
  (map-get? sponsor-tiers tier)
)

(define-read-only (get-sponsor-analytics (sponsor principal))
  (map-get? sponsor-analytics sponsor)
)

(define-read-only (calculate-sponsor-roi (sponsor principal) (event-id uint))
  (let
    (
      (sponsor-info (map-get? event-sponsors {event-id: event-id, sponsor: sponsor}))
      (analytics (map-get? sponsor-analytics sponsor))
    )
    (match sponsor-info
      info (ok {
        amount-invested: (get amount-contributed info),
        tier-level: (get tier info),
        exposure-score: (match analytics anal (get total-exposure anal) u0),
        events-sponsored: (match analytics anal (get events-sponsored anal) u0)
      })
      (ok {
        amount-invested: u0,
        tier-level: u0,
        exposure-score: u0,
        events-sponsored: u0
      })
    )
  )
)



