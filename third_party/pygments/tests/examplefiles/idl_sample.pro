; docformat = 'rst'

; Example IDL (Interactive Data Language) source code.

;+
; Get `nIndices` random indices for an array of size `nValues` (without 
; repeating an index).
;
; :Examples:
;    Try::
;
;       IDL> r = randomu(seed, 10)
;       IDL> print, r, format='(4F)'
;             0.6297589      0.7815896      0.2508559      0.7546844
;             0.1353382      0.1245834      0.8733745      0.0753110
;             0.8054136      0.9513228
;       IDL> ind = mg_sample(10, 3, seed=seed)
;       IDL> print, ind
;                  2           4           7
;       IDL> print, r[ind]
;            0.250856     0.135338    0.0753110
;
; :Returns: 
;    lonarr(`nIndices`)
;
; :Params:
;    nValues : in, required, type=long
;       size of array to choose indices from
;    nIndices : in, required, type=long
;       number of indices needed
;
; :Keywords:
;    seed : in, out, optional, type=integer or lonarr(36)
;       seed to use for random number generation, leave undefined to use a 
;       seed generated from the system clock; new seed will be output
;-
function mg_sample, nValues, nIndices, seed=seed
  compile_opt strictarr
  
  ; get random nIndices by finding the indices of the smallest nIndices in a 
  ; array of random values
  values = randomu(seed, nValues)
  
  ; our random values are uniformly distributed, so ideally the nIndices 
  ; smallest values are in the first bin of the below histogram
  nBins = nValues / nIndices
  h = histogram(values, nbins=nBins, reverse_indices=ri)

  ; the candidates for being in the first nIndices will live in bins 0..bin
  nCandidates = 0L
  for bin = 0L, nBins - 1L do begin
    nCandidates += h[bin]
    if (nCandidates ge nIndices) then break    
  endfor

  ; get the candidates and sort them
  candidates = ri[ri[0] : ri[bin + 1L] - 1L]
  sortedCandidates = sort(values[candidates])

  ; return the first nIndices of them
  return, (candidates[sortedCandidates])[0:nIndices-1L]
end


; main-level example program

r = randomu(seed, 10)            
print, r
ind = mg_sample(10, 3, seed=seed)
print, ind
print, r[ind]

end