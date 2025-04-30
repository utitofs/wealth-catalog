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
