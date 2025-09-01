;; Circular Economy Exchange - Smart Contract for Trading Recycled Materials and Upcycled Goods
;; Version: 1.0.0

;; Contract Principal
(define-constant contract-owner tx-sender)

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ITEM-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-PRICE (err u103))
(define-constant ERR-ITEM-NOT-AVAILABLE (err u104))
(define-constant ERR-SAME-USER (err u105))
(define-constant ERR-INVALID-QUANTITY (err u106))
(define-constant ERR-TRADE-NOT-FOUND (err u107))

;; Data Variables
(define-data-var item-id-counter uint u0)
(define-data-var trade-id-counter uint u0)
(define-data-var platform-fee-percentage uint u250) ;; 2.5%

;; Material Categories
(define-constant CATEGORY-PLASTIC u1)
(define-constant CATEGORY-METAL u2)
(define-constant CATEGORY-GLASS u3)
(define-constant CATEGORY-TEXTILE u4)
(define-constant CATEGORY-ELECTRONICS u5)
(define-constant CATEGORY-PAPER u6)
(define-constant CATEGORY-ORGANIC u7)

;; Item Status
(define-constant STATUS-AVAILABLE u1)
(define-constant STATUS-RESERVED u2)
(define-constant STATUS-SOLD u3)

;; Data Maps
(define-map items
  { item-id: uint }
  {
    seller: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    category: uint,
    quantity: uint,
    price-per-unit: uint,
    total-value: uint,
    status: uint,
    is-upcycled: bool,
    sustainability-score: uint,
    created-at: uint,
    updated-at: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    reputation-score: uint,
    total-trades: uint,
    items-sold: uint,
    items-bought: uint,
    eco-points: uint,
    verified: bool
  }
)

(define-map trade-records
  { trade-id: uint }
  {
    item-id: uint,
    buyer: principal,
    seller: principal,
    quantity: uint,
    price-paid: uint,
    trade-timestamp: uint,
    completed: bool
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

;; Public Functions

;; Initialize user profile
(define-public (initialize-profile)
  (begin
    (map-set user-profiles
      { user: tx-sender }
      {
        reputation-score: u100,
        total-trades: u0,
        items-sold: u0,
        items-bought: u0,
        eco-points: u10,
        verified: false
      }
    )
    (ok true)
  )
)

;; List item for sale
(define-public (list-item 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (category uint)
  (quantity uint)
  (price-per-unit uint)
  (is-upcycled bool)
  (sustainability-score uint))
  (let
    (
      (new-item-id (+ (var-get item-id-counter) u1))
      (total-value (* quantity price-per-unit))
    )
    (asserts! (> quantity u0) ERR-INVALID-QUANTITY)
    (asserts! (> price-per-unit u0) ERR-INVALID-PRICE)
    (asserts! (<= sustainability-score u100) ERR-INVALID-PRICE)
    
    (map-set items
      { item-id: new-item-id }
      {
        seller: tx-sender,
        title: title,
        description: description,
        category: category,
        quantity: quantity,
        price-per-unit: price-per-unit,
        total-value: total-value,
        status: STATUS-AVAILABLE,
        is-upcycled: is-upcycled,
        sustainability-score: sustainability-score,
        created-at: block-height,
        updated-at: block-height
      }
    )
    
    (var-set item-id-counter new-item-id)
    (award-eco-points tx-sender u5)
    (ok new-item-id)
  )
)

;; Purchase item
(define-public (purchase-item (item-id uint) (requested-quantity uint))
  (let
    (
      (item-data (unwrap! (map-get? items { item-id: item-id }) ERR-ITEM-NOT-FOUND))
      (seller (get seller item-data))
      (available-quantity (get quantity item-data))
      (price-per-unit (get price-per-unit item-data))
      (total-cost (* requested-quantity price-per-unit))
      (platform-fee (/ (* total-cost (var-get platform-fee-percentage)) u10000))
      (seller-amount (- total-cost platform-fee))
      (buyer-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender }))))
      (new-trade-id (+ (var-get trade-id-counter) u1))
    )
    
    (asserts! (not (is-eq tx-sender seller)) ERR-SAME-USER)
    (asserts! (is-eq (get status item-data) STATUS-AVAILABLE) ERR-ITEM-NOT-AVAILABLE)
    (asserts! (>= available-quantity requested-quantity) ERR-INVALID-QUANTITY)
    (asserts! (>= buyer-balance total-cost) ERR-INSUFFICIENT-BALANCE)
    
    ;; Update buyer balance
    (map-set user-balances
      { user: tx-sender }
      { balance: (- buyer-balance total-cost) }
    )
    
    ;; Update seller balance
    (let ((seller-balance (default-to u0 (get balance (map-get? user-balances { user: seller })))))
      (map-set user-balances
        { user: seller }
        { balance: (+ seller-balance seller-amount) }
      )
    )
    
    ;; Update item quantity or status
    (if (is-eq available-quantity requested-quantity)
      (map-set items
        { item-id: item-id }
        (merge item-data { status: STATUS-SOLD, updated-at: block-height })
      )
      (map-set items
        { item-id: item-id }
        (merge item-data { 
          quantity: (- available-quantity requested-quantity),
          total-value: (* (- available-quantity requested-quantity) price-per-unit),
          updated-at: block-height
        })
      )
    )
    
    ;; Record trade
    (map-set trade-records
      { trade-id: new-trade-id }
      {
        item-id: item-id,
        buyer: tx-sender,
        seller: seller,
        quantity: requested-quantity,
        price-paid: total-cost,
        trade-timestamp: block-height,
        completed: true
      }
    )
    
    (var-set trade-id-counter new-trade-id)
    
    ;; Update user profiles
    (update-user-stats tx-sender true)
    (update-user-stats seller false)
    
    ;; Award eco points
    (award-eco-points tx-sender (if (get is-upcycled item-data) u15 u10))
    (award-eco-points seller u10)
    
    (ok new-trade-id)
  )
)

;; Deposit funds to user balance
(define-public (deposit-funds)
  (let
    (
      (amount (stx-get-balance tx-sender))
      (current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender }))))
    )
    (asserts! (> amount u0) ERR-INSUFFICIENT-BALANCE)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set user-balances
      { user: tx-sender }
      { balance: (+ current-balance amount) }
    )
    
    (ok amount)
  )
)

;; Withdraw funds from user balance
(define-public (withdraw-funds (amount uint))
  (let
    (
      (current-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender }))))
    )
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (> amount u0) ERR-INVALID-PRICE)
    
    (map-set user-balances
      { user: tx-sender }
      { balance: (- current-balance amount) }
    )
    
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (ok amount)
  )
)

;; Update item details (seller only)
(define-public (update-item 
  (item-id uint)
  (new-price uint)
  (new-quantity uint)
  (new-description (string-ascii 500)))
  (let
    (
      (item-data (unwrap! (map-get? items { item-id: item-id }) ERR-ITEM-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get seller item-data)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status item-data) STATUS-AVAILABLE) ERR-ITEM-NOT-AVAILABLE)
    (asserts! (> new-price u0) ERR-INVALID-PRICE)
    (asserts! (> new-quantity u0) ERR-INVALID-QUANTITY)
    
    (map-set items
      { item-id: item-id }
      (merge item-data {
        price-per-unit: new-price,
        quantity: new-quantity,
        total-value: (* new-quantity new-price),
        description: new-description,
        updated-at: block-height
      })
    )
    
    (ok true)
  )
)

;; Remove item from marketplace (seller only)
(define-public (remove-item (item-id uint))
  (let
    (
      (item-data (unwrap! (map-get? items { item-id: item-id }) ERR-ITEM-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get seller item-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq (get status item-data) STATUS-SOLD)) ERR-ITEM-NOT-AVAILABLE)
    
    (map-delete items { item-id: item-id })
    (ok true)
  )
)

;; Private Functions

;; Update user statistics after trade
(define-private (update-user-stats (user principal) (is-buyer bool))
  (let
    (
      (current-profile (default-to 
        { reputation-score: u100, total-trades: u0, items-sold: u0, items-bought: u0, eco-points: u0, verified: false }
        (map-get? user-profiles { user: user })
      ))
    )
    (map-set user-profiles
      { user: user }
      (merge current-profile {
        total-trades: (+ (get total-trades current-profile) u1),
        items-sold: (if is-buyer (get items-sold current-profile) (+ (get items-sold current-profile) u1)),
        items-bought: (if is-buyer (+ (get items-bought current-profile) u1) (get items-bought current-profile)),
        reputation-score: (+ (get reputation-score current-profile) u5)
      })
    )
  )
)

;; Award eco points to users
(define-private (award-eco-points (user principal) (points uint))
  (let
    (
      (current-profile (default-to 
        { reputation-score: u100, total-trades: u0, items-sold: u0, items-bought: u0, eco-points: u0, verified: false }
        (map-get? user-profiles { user: user })
      ))
    )
    (map-set user-profiles
      { user: user }
      (merge current-profile {
        eco-points: (+ (get eco-points current-profile) points)
      })
    )
  )
)

;; Read-only Functions

;; Get item details
(define-read-only (get-item (item-id uint))
  (map-get? items { item-id: item-id })
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

;; Get user balance
(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

;; Get trade record
(define-read-only (get-trade-record (trade-id uint))
  (map-get? trade-records { trade-id: trade-id })
)

;; Get current item counter
(define-read-only (get-item-counter)
  (var-get item-id-counter)
)

;; Get current trade counter
(define-read-only (get-trade-counter)
  (var-get trade-id-counter)
)

;; Get items by category
(define-read-only (get-items-by-status (status uint))
  (var-get item-id-counter) ;; Returns counter for iteration reference
)

;; Get platform fee percentage
(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

;; Admin Functions

;; Update platform fee (contract owner only)
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR-INVALID-PRICE) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

;; Verify user (contract owner only)
(define-public (verify-user (user principal))
  (let
    (
      (current-profile (unwrap! (map-get? user-profiles { user: user }) ERR-ITEM-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    
    (map-set user-profiles
      { user: user }
      (merge current-profile { verified: true })
    )
    
    (award-eco-points user u50)
    (ok true)
  )
)

;; Emergency pause item (contract owner only)
(define-public (emergency-pause-item (item-id uint))
  (let
    (
      (item-data (unwrap! (map-get? items { item-id: item-id }) ERR-ITEM-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    
    (map-set items
      { item-id: item-id }
      (merge item-data { status: STATUS-RESERVED })
    )
    
    (ok true)
  )
)

;; Utility Functions

;; Calculate environmental impact score
(define-read-only (calculate-impact-score (item-id uint))
  (match (map-get? items { item-id: item-id })
    item-data 
      (let
        (
          (base-score (get sustainability-score item-data))
          (upcycle-bonus (if (get is-upcycled item-data) u20 u0))
          (category-bonus (if (is-eq (get category item-data) CATEGORY-ELECTRONICS) u15 u10))
        )
        (ok (+ base-score upcycle-bonus category-bonus))
      )
    ERR-ITEM-NOT-FOUND
  )
)

;; Check if user can afford item
(define-read-only (can-afford-item (user principal) (item-id uint) (quantity uint))
  (match (map-get? items { item-id: item-id })
    item-data
      (let
        (
          (total-cost (* quantity (get price-per-unit item-data)))
          (user-balance (get-user-balance user))
        )
        (ok (>= user-balance total-cost))
      )
    ERR-ITEM-NOT-FOUND
  )
)

;; Get marketplace statistics
(define-read-only (get-marketplace-stats)
  {
    total-items: (var-get item-id-counter),
    total-trades: (var-get trade-id-counter),
    platform-fee: (var-get platform-fee-percentage)
  }
)

;; Search items by category and status
(define-read-only (search-items (category uint) (min-sustainability uint))
  (ok {
    category: category,
    min-sustainability: min-sustainability,
    total-items: (var-get item-id-counter)
  })
)

;; Contract Initialization
(begin
  (try! (initialize-profile))
  (print "Circular Economy Exchange initialized successfully")
)