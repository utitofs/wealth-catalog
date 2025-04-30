;; Wealth Catalog Management System
;; This smart contract facilitates the organized tracking, permission management, and maintenance of a collection of unique virtual treasures on the blockchain.
;; Each treasure includes metadata such as identifier, dimensions, authorship, and categorization information.

;; -----------------------------
;; SYSTEM CONFIGURATION PARAMETERS
;; -----------------------------
(define-constant SYSTEM-ADMINISTRATOR tx-sender)  ;; The administrative entity for this contract

;; Operational status codes for system responses
(define-constant STATUS-ITEM-MISSING (err u301))           ;; Response when requested item cannot be located
(define-constant STATUS-ADMIN-RESTRICTED (err u307))       ;; Response when operation is limited to administrative users
(define-constant STATUS-ITEM-ALREADY-EXISTS (err u302))    ;; Response when attempting to register a pre-existing item
(define-constant STATUS-INVALID-NAME-FORMAT (err u303))    ;; Response when name format violates system requirements
(define-constant STATUS-INVALID-DIMENSION (err u304))      ;; Response when dimensional data is out of acceptable range
(define-constant STATUS-INSUFFICIENT-PRIVILEGES (err u305));; Response when operation requires elevated permissions
(define-constant STATUS-DESTINATION-INVALID (err u306))    ;; Response when specified recipient is invalid
(define-constant STATUS-NO-VIEW-RIGHTS (err u308))         ;; Response when viewing permissions are not granted

;; -----------------------------
;; PERSISTENT DATA STRUCTURES
;; -----------------------------
;; Counter for total registered items in the system
(define-data-var registry-item-count uint u0)

;; Primary data repository for individual registered items
(define-map item-repository
  { item-id: uint }  ;; Each item identified by a unique numerical identifier
  {
    name: (string-ascii 64),              ;; Official designation of the item
    author: principal,                    ;; Original creator of the item
    dimension: uint,                      ;; Dimensional measurement value
    registration-block: uint,             ;; Block height when item was registered
    details: (string-ascii 128),          ;; Extended information about the item
    categories: (list 10 (string-ascii 32)) ;; Classification tags for the item
  }
)

;; Permission management system for individual items
(define-map permission-registry
  { item-id: uint, subject: principal }  ;; Item and user combination
  { can-view: bool }                     ;; Viewing permission status
)

;; -----------------------------
;; UTILITY FUNCTIONS
;; -----------------------------
;; Verifies existence of specified item in the repository
(define-private (item-exists? (item-id uint))
  (is-some (map-get? item-repository { item-id: item-id }))
)

;; Confirms whether specified user is the author of an item
(define-private (is-item-author? (item-id uint) (author principal))
  (match (map-get? item-repository { item-id: item-id })
    item-record (is-eq (get author item-record) author)
    false
  )
)

;; Retrieves dimensional value for specified item
(define-private (get-item-dimension (item-id uint))
  (default-to u0 
    (get dimension 
      (map-get? item-repository { item-id: item-id })
    )
  )
)

;; Validates format compliance for individual category tag
(define-private (is-valid-category? (category (string-ascii 32)))
  (and 
    (> (len category) u0)     ;; Category must have minimum length
    (< (len category) u33)    ;; Category must not exceed maximum length
  )
)

;; Performs collective validation on all category tags
(define-private (are-categories-valid? (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)                 ;; Must have at least one category
    (<= (len categories) u10)               ;; Must not exceed maximum categories
    (is-eq (len (filter is-valid-category? categories)) (len categories))  ;; All categories must pass validation
  )
)

;; Validates string length against minimum and maximum constraints
(define-private (verify-text-length (value (string-ascii 64)) (min-len uint) (max-len uint))
  (and 
    (>= (len value) min-len)
    (<= (len value) max-len)
  )
)

;; Updates the registry counter and returns previous value
(define-private (increment-item-counter)
  (let ((current-value (var-get registry-item-count)))
    (var-set registry-item-count (+ current-value u1))
    (ok current-value) ;; Returns pre-increment value
  )
)

;; -----------------------------
;; PUBLIC INTERFACE FUNCTIONS
;; -----------------------------
;; Creates and registers a new item in the system
(define-public (register-item (name (string-ascii 64)) (dimension uint) (details (string-ascii 128)) (categories (list 10 (string-ascii 32))))
  (let
    (
      (new-item-id (+ (var-get registry-item-count) u1))  ;; Generate new sequential identifier
    )
    ;; Input validation sequence
    (asserts! (and (> (len name) u0) (< (len name) u65)) STATUS-INVALID-NAME-FORMAT)  ;; Validate name length
    (asserts! (and (> dimension u0) (< dimension u1000000000)) STATUS-INVALID-DIMENSION)  ;; Validate dimension range
    (asserts! (and (> (len details) u0) (< (len details) u129)) STATUS-INVALID-NAME-FORMAT)  ;; Validate details length
    (asserts! (are-categories-valid? categories) STATUS-INVALID-NAME-FORMAT)  ;; Validate categories collection

    ;; Persist the new item record
    (map-insert item-repository
      { item-id: new-item-id }
      {
        name: name,
        author: tx-sender,
        dimension: dimension,
        registration-block: block-height,
        details: details,
        categories: categories
      }
    )

    ;; Establish initial permission (creator automatically granted access)
    (map-insert permission-registry
      { item-id: new-item-id, subject: tx-sender }
      { can-view: true }
    )

    ;; Update the registry counter
    (var-set registry-item-count new-item-id)
    (ok new-item-id)  ;; Return the newly assigned identifier
  )
)

;; Retrieves detailed information about a specific item
(define-public (fetch-item-details (item-id uint))
  ;; Retrieves the descriptive information for an item
  (let
    (
      (item-record (unwrap! (map-get? item-repository { item-id: item-id }) STATUS-ITEM-MISSING))
    )
    (ok (get details item-record))
  )
)

;; Verifies user access permissions for a specific item
(define-public (verify-access-permission (item-id uint) (subject principal))
  ;; Confirms whether the specified user has viewing rights
  (let
    (
      (permission-data (map-get? permission-registry { item-id: item-id, subject: subject }))
    )
    (ok (is-some permission-data))
  )
)

;; Counts the number of categories assigned to an item
(define-public (count-item-categories (item-id uint))
  ;; Returns the total number of categorization tags for an item
  (let
    (
      (item-record (unwrap! (map-get? item-repository { item-id: item-id }) STATUS-ITEM-MISSING))
    )
    (ok (len (get categories item-record)))
  )
)

;; Validates name string against system requirements
(define-public (validate-name-format (name (string-ascii 64)))
  ;; Confirms name meets length requirements
  (ok (and (> (len name) u0) (<= (len name) u64)))
)

;; Transfers item ownership to a new account
(define-public (transfer-item-ownership (item-id uint) (new-author principal))
  (let
    (
      (item-record (unwrap! (map-get? item-repository { item-id: item-id }) STATUS-ITEM-MISSING))
    )
    (asserts! (item-exists? item-id) STATUS-ITEM-MISSING)  ;; Verify item exists
    (asserts! (is-eq (get author item-record) tx-sender) STATUS-INSUFFICIENT-PRIVILEGES)  ;; Verify sender is current owner

    ;; Update ownership record
    (map-set item-repository
      { item-id: item-id }
      (merge item-record { author: new-author })  ;; Update ownership field
    )
    (ok true)  ;; Confirm successful transfer
  )
)

;; Updates metadata for an existing item
(define-public (modify-item (item-id uint) (new-name (string-ascii 64)) (new-dimension uint) (new-details (string-ascii 128)) (new-categories (list 10 (string-ascii 32))))
  (let
    (
      (item-record (unwrap! (map-get? item-repository { item-id: item-id }) STATUS-ITEM-MISSING))
    )
    ;; Comprehensive validation checks
    (asserts! (item-exists? item-id) STATUS-ITEM-MISSING)  ;; Verify item exists
    (asserts! (is-eq (get author item-record) tx-sender) STATUS-INSUFFICIENT-PRIVILEGES)  ;; Verify modification authority
    (asserts! (and (> (len new-name) u0) (< (len new-name) u65)) STATUS-INVALID-NAME-FORMAT)  ;; Validate new name
    (asserts! (and (> new-dimension u0) (< new-dimension u1000000000)) STATUS-INVALID-DIMENSION)  ;; Validate new dimension
    (asserts! (and (> (len new-details) u0) (< (len new-details) u129)) STATUS-INVALID-NAME-FORMAT)  ;; Validate new details
    (asserts! (are-categories-valid? new-categories) STATUS-INVALID-NAME-FORMAT)  ;; Validate new categories

    ;; Update item record with new metadata
    (map-set item-repository
      { item-id: item-id }
      (merge item-record { 
        name: new-name, 
        dimension: new-dimension, 
        details: new-details, 
        categories: new-categories 
      })
    )
    (ok true)  ;; Confirm successful update
  )
)

;; Permanently removes an item from the repository
(define-public (remove-item (item-id uint))
  (let
    (
      (item-record (unwrap! (map-get? item-repository { item-id: item-id }) STATUS-ITEM-MISSING))
    )
    (asserts! (item-exists? item-id) STATUS-ITEM-MISSING)  ;; Verify item exists
    (asserts! (is-eq (get author item-record) tx-sender) STATUS-INSUFFICIENT-PRIVILEGES)  ;; Verify removal authority

    ;; Remove item from repository
    (map-delete item-repository { item-id: item-id })
    (ok true)  ;; Confirm successful removal
  )
)

