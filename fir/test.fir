(let ((b : bool #f))
  (if (%gt 2 6)
    ((if b %add %gt) 1 3)
    2))
