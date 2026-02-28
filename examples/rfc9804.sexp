(certificate
  (issuer "Example CA")
  (subject "alice")
  (not-before "2025-01-01")
  (not-after "2026-12-31")
  (public-key
    (algorithm ed25519)
    (key-data |MCowBQYDK2VwAyEA|))
  (fingerprint #A1B2C3D4E5F6#)
  (serial 42)
  (permissions
    (tag
      (http //*.example.net/*)
      (ftp //*.example.net/*)))
  (signature
    [3:sig]|MEUCIQC+n2BFg|))
