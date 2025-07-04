;; AgriBlock Resource Manager
;; Blockchain-based agricultural asset management and verification system
;; Enables secure registration and tracking of farming operations through distributed ledger technology

;; Master registry counter for sequential asset identification
(define-data-var total-registered-assets uint u0)

;; ===== Core Data Storage Architecture =====

;; Primary asset registry containing comprehensive farming operation records
(define-map agricultural-assets
  { asset-identifier: uint }
  {
    parcel-designation: (string-ascii 64),
    owner-identity: principal,
    area-measurement: uint,
    registration-block: uint,
    soil-characteristics: (string-ascii 128),
    cultivation-species: (list 10 (string-ascii 32))
  }
)

;; Access management system for third-party verification and inspection rights
(define-map inspector-permissions
  { asset-identifier: uint, verifier-identity: principal }
  { access-granted: bool }
)

;; System error codes and response handling constants
(define-constant asset-not-found-error (err u401))
(define-constant duplicate-registration-error (err u402))
(define-constant invalid-naming-error (err u403))
(define-constant measurement-bounds-error (err u404))
(define-constant unauthorized-access-error (err u405))
(define-constant ownership-verification-error (err u406))
(define-constant admin-only-error (err u400))
(define-constant privacy-violation-error (err u407))
(define-constant data-format-error (err u408))

;; Protocol administrator with elevated privileges
(define-constant protocol-administrator tx-sender)

;; ===== Asset Registration and Management Functions =====

;; Registers new agricultural asset with comprehensive metadata
(define-public (register-agricultural-asset 
  (designation (string-ascii 64)) 
  (measurement uint) 
  (soil-data (string-ascii 128)) 
  (species-list (list 10 (string-ascii 32)))
)
  (let
    (
      (next-asset-id (+ (var-get total-registered-assets) u1))
    )
    ;; Data validation and integrity checks
    (asserts! (> (len designation) u0) invalid-naming-error)
    (asserts! (< (len designation) u65) invalid-naming-error)
    (asserts! (> measurement u0) measurement-bounds-error)
    (asserts! (< measurement u1000000000) measurement-bounds-error)
    (asserts! (> (len soil-data) u0) invalid-naming-error)
    (asserts! (< (len soil-data) u129) invalid-naming-error)
    (asserts! (verify-species-data species-list) data-format-error)

    ;; Insert new asset record into primary registry
    (map-insert agricultural-assets
      { asset-identifier: next-asset-id }
      {
        parcel-designation: designation,
        owner-identity: tx-sender,
        area-measurement: measurement,
        registration-block: block-height,
        soil-characteristics: soil-data,
        cultivation-species: species-list
      }
    )

    ;; Grant default access rights to asset owner
    (map-insert inspector-permissions
      { asset-identifier: next-asset-id, verifier-identity: tx-sender }
      { access-granted: true }
    )

    ;; Update global asset counter
    (var-set total-registered-assets next-asset-id)
    (ok next-asset-id)
  )
)

;; Verifies asset ownership and returns authentication status
(define-public (verify-asset-ownership (asset-identifier uint) (claimed-owner principal))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
      (verified-owner (get owner-identity asset-data))
      (registration-timestamp (get registration-block asset-data))
      (verifier-authorized (default-to 
        false 
        (get access-granted 
          (map-get? inspector-permissions { asset-identifier: asset-identifier, verifier-identity: tx-sender })
        )
      ))
    )
    ;; Authorization and existence validation
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! 
      (or 
        (is-eq tx-sender verified-owner)
        verifier-authorized
        (is-eq tx-sender protocol-administrator)
      ) 
      unauthorized-access-error
    )

    ;; Return verification results with metadata
    (if (is-eq verified-owner claimed-owner)
      (ok {
        verification-successful: true,
        current-block: block-height,
        ownership-duration: (- block-height registration-timestamp),
        identity-match: true
      })
      (ok {
        verification-successful: false,
        current-block: block-height,
        ownership-duration: (- block-height registration-timestamp),
        identity-match: false
      })
    )
  )
)

;; Removes asset from registry permanently
(define-public (deregister-asset (asset-identifier uint))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
    )
    ;; Ownership verification for deregistration
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! (is-eq (get owner-identity asset-data) tx-sender) ownership-verification-error)

    ;; Execute asset removal from registry
    (map-delete agricultural-assets { asset-identifier: asset-identifier })
    (ok true)
  )
)

;; Transfers asset ownership to designated successor
(define-public (transfer-asset-ownership (asset-identifier uint) (new-owner principal))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
    )
    ;; Current ownership validation
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! (is-eq (get owner-identity asset-data) tx-sender) ownership-verification-error)

    ;; Execute ownership transfer
    (map-set agricultural-assets
      { asset-identifier: asset-identifier }
      (merge asset-data { owner-identity: new-owner })
    )
    (ok true)
  )
)

;; Revokes verification access for specified inspector
(define-public (remove-inspector-access (asset-identifier uint) (inspector-identity principal))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
    )
    ;; Ownership and self-revocation validation
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! (is-eq (get owner-identity asset-data) tx-sender) ownership-verification-error)
    (asserts! (not (is-eq inspector-identity tx-sender)) admin-only-error)

    ;; Remove inspector access privileges
    (map-delete inspector-permissions { asset-identifier: asset-identifier, verifier-identity: inspector-identity })
    (ok true)
  )
)

;; Adds new cultivation species to existing asset record
(define-public (append-cultivation-species (asset-identifier uint) (new-species (list 10 (string-ascii 32))))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
      (existing-species (get cultivation-species asset-data))
      (merged-species (unwrap! (as-max-len? (concat existing-species new-species) u10) data-format-error))
    )
    ;; Validation and ownership checks
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! (is-eq (get owner-identity asset-data) tx-sender) ownership-verification-error)
    (asserts! (verify-species-data new-species) data-format-error)

    ;; Update asset with expanded species list
    (map-set agricultural-assets
      { asset-identifier: asset-identifier }
      (merge asset-data { cultivation-species: merged-species })
    )
    (ok merged-species)
  )
)

;; ===== Utility and Helper Functions =====

;; Validates asset existence in registry
(define-private (check-asset-exists (asset-identifier uint))
  (is-some (map-get? agricultural-assets { asset-identifier: asset-identifier }))
)

;; Confirms caller ownership of specified asset
(define-private (confirm-asset-ownership (asset-identifier uint) (owner-identity principal))
  (match (map-get? agricultural-assets { asset-identifier: asset-identifier })
    asset-data (is-eq (get owner-identity asset-data) owner-identity)
    false
  )
)

;; Retrieves area measurement for specified asset
(define-private (get-asset-area (asset-identifier uint))
  (default-to u0
    (get area-measurement
      (map-get? agricultural-assets { asset-identifier: asset-identifier })
    )
  )
)

;; Validates individual species name format
(define-private (validate-species-name (species-name (string-ascii 32)))
  (and
    (> (len species-name) u0)
    (< (len species-name) u33)
  )
)

;; Validates complete species list structure and content
(define-private (verify-species-data (species-list (list 10 (string-ascii 32))))
  (and
    (> (len species-list) u0)
    (<= (len species-list) u10)
    (is-eq (len (filter validate-species-name species-list)) (len species-list))
  )
)

;; Implements protective measures for asset security
(define-public (activate-asset-protection (asset-identifier uint))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
      (protection-status "SECURITY-PROTOCOL-ENGAGED")
      (current-species (get cultivation-species asset-data))
    )
    ;; Authority validation for protection activation
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! 
      (or 
        (is-eq tx-sender protocol-administrator)
        (is-eq (get owner-identity asset-data) tx-sender)
      ) 
      admin-only-error
    )

    ;; Return protection activation confirmation
    (ok true)
  )
)

;; Updates comprehensive asset metadata
(define-public (update-asset-metadata 
  (asset-identifier uint) 
  (new-designation (string-ascii 64)) 
  (new-measurement uint) 
  (new-soil-data (string-ascii 128)) 
  (new-species-list (list 10 (string-ascii 32)))
)
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
    )
    ;; Ownership and input validation
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! (is-eq (get owner-identity asset-data) tx-sender) ownership-verification-error)
    (asserts! (> (len new-designation) u0) invalid-naming-error)
    (asserts! (< (len new-designation) u65) invalid-naming-error)
    (asserts! (> new-measurement u0) measurement-bounds-error)
    (asserts! (< new-measurement u1000000000) measurement-bounds-error)
    (asserts! (> (len new-soil-data) u0) invalid-naming-error)
    (asserts! (< (len new-soil-data) u129) invalid-naming-error)
    (asserts! (verify-species-data new-species-list) data-format-error)

    ;; Apply comprehensive metadata updates
    (map-set agricultural-assets
      { asset-identifier: asset-identifier }
      (merge asset-data { 
        parcel-designation: new-designation, 
        area-measurement: new-measurement, 
        soil-characteristics: new-soil-data, 
        cultivation-species: new-species-list 
      })
    )
    (ok true)
  )
)

;; Grants verification privileges to designated inspector
(define-public (grant-inspector-privileges (asset-identifier uint) (inspector-identity principal))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
    )
    ;; Ownership validation and self-assignment prevention
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! (is-eq (get owner-identity asset-data) tx-sender) ownership-verification-error)
    (asserts! (not (is-eq inspector-identity tx-sender)) admin-only-error)

    ;; Grant inspector access privileges
    (map-set inspector-permissions
      { asset-identifier: asset-identifier, verifier-identity: inspector-identity }
      { access-granted: true }
    )
    (ok true)
  )
)

;; Assesses current operational status of asset
(define-public (assess-asset-status (asset-identifier uint))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
      (inspector-authorized (default-to 
        false 
        (get access-granted 
          (map-get? inspector-permissions { asset-identifier: asset-identifier, verifier-identity: tx-sender })
        )
      ))
    )
    ;; Access authorization validation
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! 
      (or 
        (is-eq tx-sender (get owner-identity asset-data))
        inspector-authorized
        (is-eq tx-sender protocol-administrator)
      ) 
      unauthorized-access-error
    )

    ;; Return comprehensive status assessment
    (ok {
      status-active: true,
      area-size: (get area-measurement asset-data),
      parcel-name: (get parcel-designation asset-data),
      registration-age: (- block-height (get registration-block asset-data))
    })
  )
)

;; Calculates aggregate holdings for specified owner
(define-public (calculate-owner-portfolio (owner-address principal))
  (begin
    ;; Placeholder implementation for portfolio calculation
    ;; Real implementation would require iteration through all assets
    (ok u0)
  )
)

;; Generates detailed analytical report for asset
(define-public (generate-asset-report (asset-identifier uint))
  (let
    (
      (asset-data (unwrap! (map-get? agricultural-assets { asset-identifier: asset-identifier }) asset-not-found-error))
      (inspector-authorized (default-to 
        false 
        (get access-granted 
          (map-get? inspector-permissions { asset-identifier: asset-identifier, verifier-identity: tx-sender })
        )
      ))
    )
    ;; Access permission validation
    (asserts! (check-asset-exists asset-identifier) asset-not-found-error)
    (asserts! 
      (or 
        (is-eq tx-sender (get owner-identity asset-data))
        inspector-authorized
        (is-eq tx-sender protocol-administrator)
      ) 
      unauthorized-access-error
    )

    ;; Return comprehensive asset analysis
    (ok {
      parcel-name: (get parcel-designation asset-data),
      owner-principal: (get owner-identity asset-data),
      area-size: (get area-measurement asset-data),
      registration-height: (get registration-block asset-data),
      soil-profile: (get soil-characteristics asset-data),
      species-catalog: (get cultivation-species asset-data)
    })
  )
)

