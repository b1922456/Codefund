;; Bounty Statistics Tracker
;; Tracks bounty completion rates, developer performance metrics, and platform analytics

(define-constant err-not-found (err u404))
(define-constant err-unauthorized (err u403))
(define-constant contract-owner tx-sender)

;; Monthly platform statistics
(define-map monthly-stats
  { year: uint, month: uint }
  {
    total-bounties-created: uint,
    bounties-completed: uint,
    total-funds-distributed: uint,
    active-developers: uint,
    avg-completion-time: uint,
    success-rate: uint
  }
)

;; Developer performance metrics
(define-map developer-metrics
  { developer: principal, month: uint, year: uint }
  {
    bounties-started: uint,
    bounties-completed: uint,
    total-earned: uint,
    avg-completion-time: uint,
    rejection-rate: uint,
    rating-score: uint
  }
)

;; Bounty category statistics
(define-map category-stats
  { category: (string-ascii 50) }
  {
    total-bounties: uint,
    completed-bounties: uint,
    avg-reward-amount: uint,
    avg-completion-days: uint,
    top-performers: (list 3 principal)
  }
)

;; Current tracking variables
(define-data-var current-month uint u1)
(define-data-var current-year uint u2024)
(define-data-var total-platform-volume uint u0)

;; Record bounty creation
(define-public (record-bounty-created (creator principal) (category (string-ascii 50)) (reward-amount uint))
  (let 
    (
      (month (var-get current-month))
      (year (var-get current-year))
      (current-monthly (default-to 
        { total-bounties-created: u0, bounties-completed: u0, total-funds-distributed: u0, 
          active-developers: u0, avg-completion-time: u0, success-rate: u0 }
        (map-get? monthly-stats { year: year, month: month })))
      (current-category (default-to 
        { total-bounties: u0, completed-bounties: u0, avg-reward-amount: u0, 
          avg-completion-days: u0, top-performers: (list) }
        (map-get? category-stats { category: category })))
    )
    
    ;; Update monthly statistics
    (map-set monthly-stats
      { year: year, month: month }
      (merge current-monthly {
        total-bounties-created: (+ (get total-bounties-created current-monthly) u1)
      })
    )
    
    ;; Update category statistics
    (map-set category-stats
      { category: category }
      (merge current-category {
        total-bounties: (+ (get total-bounties current-category) u1),
        avg-reward-amount: (/ (+ (* (get avg-reward-amount current-category) (get total-bounties current-category)) reward-amount)
                             (+ (get total-bounties current-category) u1))
      })
    )
    
    (ok true)
  )
)

;; Record bounty completion
(define-public (record-bounty-completed (developer principal) (category (string-ascii 50)) (reward-amount uint) (days-to-complete uint))
  (let 
    (
      (month (var-get current-month))
      (year (var-get current-year))
      (current-monthly (default-to 
        { total-bounties-created: u0, bounties-completed: u0, total-funds-distributed: u0, 
          active-developers: u0, avg-completion-time: u0, success-rate: u0 }
        (map-get? monthly-stats { year: year, month: month })))
      (dev-metrics (default-to 
        { bounties-started: u0, bounties-completed: u0, total-earned: u0, 
          avg-completion-time: u0, rejection-rate: u0, rating-score: u0 }
        (map-get? developer-metrics { developer: developer, month: month, year: year })))
      (current-category (default-to 
        { total-bounties: u0, completed-bounties: u0, avg-reward-amount: u0, 
          avg-completion-days: u0, top-performers: (list) }
        (map-get? category-stats { category: category })))
    )
    
    ;; Update monthly statistics
    (map-set monthly-stats
      { year: year, month: month }
      (merge current-monthly {
        bounties-completed: (+ (get bounties-completed current-monthly) u1),
        total-funds-distributed: (+ (get total-funds-distributed current-monthly) reward-amount),
        success-rate: (if (> (get total-bounties-created current-monthly) u0)
                        (/ (* (+ (get bounties-completed current-monthly) u1) u100) 
                           (get total-bounties-created current-monthly))
                        u0)
      })
    )
    
    ;; Update developer metrics
    (map-set developer-metrics
      { developer: developer, month: month, year: year }
      (merge dev-metrics {
        bounties-completed: (+ (get bounties-completed dev-metrics) u1),
        total-earned: (+ (get total-earned dev-metrics) reward-amount),
        avg-completion-time: (if (> (get bounties-completed dev-metrics) u0)
                               (/ (+ (* (get avg-completion-time dev-metrics) (get bounties-completed dev-metrics)) days-to-complete)
                                  (+ (get bounties-completed dev-metrics) u1))
                               days-to-complete)
      })
    )
    
    ;; Update category statistics
    (map-set category-stats
      { category: category }
      (merge current-category {
        completed-bounties: (+ (get completed-bounties current-category) u1),
        avg-completion-days: (if (> (get completed-bounties current-category) u0)
                               (/ (+ (* (get avg-completion-days current-category) (get completed-bounties current-category)) days-to-complete)
                                  (+ (get completed-bounties current-category) u1))
                               days-to-complete)
      })
    )
    
    ;; Update total platform volume
    (var-set total-platform-volume (+ (var-get total-platform-volume) reward-amount))
    
    (ok true)
  )
)

;; Update current period (admin function)
(define-public (update-period (month uint) (year uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set current-month month)
    (var-set current-year year)
    (ok true)
  )
)

;; Calculate developer rank based on performance
(define-public (calculate-developer-rank (developer principal))
  (let 
    (
      (month (var-get current-month))
      (year (var-get current-year))
      (metrics (unwrap! (map-get? developer-metrics { developer: developer, month: month, year: year }) err-not-found))
      (completion-score (* (get bounties-completed metrics) u10))
      (earnings-score (/ (get total-earned metrics) u100))
      (speed-score (if (> (get avg-completion-time metrics) u0) 
                     (/ u3000 (get avg-completion-time metrics)) 
                     u0))
      (total-rank (+ completion-score earnings-score speed-score))
    )
    (ok total-rank)
  )
)

;; Get top performers for a category
(define-read-only (get-top-performers (category (string-ascii 50)))
  (map-get? category-stats { category: category })
)

;; Get monthly platform statistics
(define-read-only (get-monthly-stats (year uint) (month uint))
  (map-get? monthly-stats { year: year, month: month })
)

;; Get developer performance metrics
(define-read-only (get-developer-metrics (developer principal) (month uint) (year uint))
  (map-get? developer-metrics { developer: developer, month: month, year: year })
)

;; Get category performance statistics
(define-read-only (get-category-stats (category (string-ascii 50)))
  (map-get? category-stats { category: category })
)

;; Get platform overview
(define-read-only (get-platform-overview)
  {
    total-volume: (var-get total-platform-volume),
    current-period: { month: (var-get current-month), year: (var-get current-year) }
  }
)

;; Calculate success rate for a developer
(define-read-only (calculate-developer-success-rate (developer principal) (month uint) (year uint))
  (match (map-get? developer-metrics { developer: developer, month: month, year: year })
    metrics (if (> (get bounties-started metrics) u0)
              (/ (* (get bounties-completed metrics) u100) (get bounties-started metrics))
              u0)
    u0
  )
)
