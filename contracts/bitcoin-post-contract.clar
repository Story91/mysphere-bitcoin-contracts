;; BitcoinPostContract - Clarity Smart Contract for Stacks/Bitcoin
;; Version: 1.0.0
;; MySphere Social Platform - Bitcoin Layer 2 Integration
;; Dokumentacja Clarity: https://docs.stacks.co/clarity/overview

;; ==========================================
;; Constants
;; ==========================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_POST_NOT_FOUND (err u101))
(define-constant ERR_INVALID_CONTENT (err u102))
(define-constant ERR_CONTRACT_PAUSED (err u103))
(define-constant ERR_ALREADY_LIKED (err u104))
(define-constant ERR_NOT_LIKED (err u105))

;; Maximum content hash length (IPFS CID)
(define-constant MAX_CONTENT_LENGTH u256)

;; ==========================================
;; Data Variables
;; ==========================================

;; Contract pause state
(define-data-var contract-paused bool false)

;; Total posts counter
(define-data-var total-posts uint u0)

;; ==========================================
;; Data Maps
;; ==========================================

;; Store post data
(define-map posts
  { post-id: uint }
  {
    creator: principal,
    content-hash: (string-utf8 256),
    timestamp: uint,
    likes: uint
  }
)

;; Track posts per user
(define-map user-post-count
  { user: principal }
  { count: uint }
)

;; Track user posts by index
(define-map user-posts
  { user: principal, index: uint }
  { post-id: uint }
)

;; Track likes per post per user (prevent double likes)
(define-map post-likes
  { post-id: uint, user: principal }
  { liked: bool }
)

;; ==========================================
;; Read-Only Functions
;; ==========================================

;; Get total posts count
(define-read-only (get-total-posts)
  (ok (var-get total-posts))
)

;; Get post by ID
(define-read-only (get-post (post-id uint))
  (match (map-get? posts { post-id: post-id })
    post (ok post)
    ERR_POST_NOT_FOUND
  )
)

;; Get user's post count
(define-read-only (get-user-post-count (user principal))
  (default-to { count: u0 } (map-get? user-post-count { user: user }))
)

;; Check if user has liked a post
(define-read-only (has-user-liked (post-id uint) (user principal))
  (default-to { liked: false } (map-get? post-likes { post-id: post-id, user: user }))
)

;; Check if contract is paused
(define-read-only (is-paused)
  (ok (var-get contract-paused))
)

;; Get contract owner
(define-read-only (get-owner)
  (ok CONTRACT_OWNER)
)

;; ==========================================
;; Public Functions
;; ==========================================

;; Create a new post
(define-public (create-post (content-hash (string-utf8 256)))
  (let
    (
      (post-id (+ (var-get total-posts) u1))
      (user-posts-count (get count (get-user-post-count tx-sender)))
    )
    ;; Check if contract is paused
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    
    ;; Validate content hash is not empty
    (asserts! (> (len content-hash) u0) ERR_INVALID_CONTENT)
    
    ;; Store the post
    (map-set posts
      { post-id: post-id }
      {
        creator: tx-sender,
        content-hash: content-hash,
        timestamp: block-height,
        likes: u0
      }
    )
    
    ;; Update user's post count
    (map-set user-post-count
      { user: tx-sender }
      { count: (+ user-posts-count u1) }
    )
    
    ;; Store reference to user's posts
    (map-set user-posts
      { user: tx-sender, index: user-posts-count }
      { post-id: post-id }
    )
    
    ;; Increment total posts
    (var-set total-posts post-id)
    
    ;; Emit event
    (print {
      event: "post-created",
      post-id: post-id,
      creator: tx-sender,
      content-hash: content-hash,
      timestamp: block-height
    })
    
    ;; Return post ID
    (ok post-id)
  )
)

;; Like a post
(define-public (like-post (post-id uint))
  (let
    (
      (post (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND))
      (already-liked (get liked (has-user-liked post-id tx-sender)))
    )
    ;; Check if contract is paused
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    
    ;; Check if already liked
    (asserts! (not already-liked) ERR_ALREADY_LIKED)
    
    ;; Update post likes
    (map-set posts
      { post-id: post-id }
      (merge post { likes: (+ (get likes post) u1) })
    )
    
    ;; Record the like
    (map-set post-likes
      { post-id: post-id, user: tx-sender }
      { liked: true }
    )
    
    ;; Emit event
    (print {
      event: "post-liked",
      post-id: post-id,
      user: tx-sender
    })
    
    (ok true)
  )
)

;; Unlike a post
(define-public (unlike-post (post-id uint))
  (let
    (
      (post (unwrap! (map-get? posts { post-id: post-id }) ERR_POST_NOT_FOUND))
      (already-liked (get liked (has-user-liked post-id tx-sender)))
    )
    ;; Check if contract is paused
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    
    ;; Check if actually liked
    (asserts! already-liked ERR_NOT_LIKED)
    
    ;; Update post likes
    (map-set posts
      { post-id: post-id }
      (merge post { likes: (- (get likes post) u1) })
    )
    
    ;; Remove the like
    (map-set post-likes
      { post-id: post-id, user: tx-sender }
      { liked: false }
    )
    
    ;; Emit event
    (print {
      event: "post-unliked",
      post-id: post-id,
      user: tx-sender
    })
    
    (ok true)
  )
)

;; ==========================================
;; Admin Functions
;; ==========================================

;; Pause the contract (only owner)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused true)
    (print { event: "contract-paused" })
    (ok true)
  )
)

;; Unpause the contract (only owner)
(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused false)
    (print { event: "contract-unpaused" })
    (ok true)
  )
)

