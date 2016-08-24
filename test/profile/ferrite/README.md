# Crash-Consistency Verification and Synthesis

This directory contains a crash consistency verification and synthesis example drawn from section 6 of [our ASPLOS'16 paper on Ferrites][ferrite].

The two tests `rename.rkt` and `chrome.rkt` are different litmus tests.

The slow and fast variants of this code differ in two ways:
1. In `lang.rkt`, the `inode-op` structs are opaque in the fast variant to prevent merging
2. In `ext4.rkt`, the `size` member of the `file` struct is a mutable box accessed using `for/all` in the fast variant

The slow variant of `chrome.rkt` does not return in reasonable time.