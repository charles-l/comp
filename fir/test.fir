(let ((b : bool #t) (d : int 3))
  (let ((a : int (%add d 4)))
    (if b
      a
      2)))
