# Crash-Consistency Verification and Synthesis

This directory contains two ways to implement crash consistency verification and synthesis as described in section 6 of [our ASPLOS'16 paper][ferrite].

Each directory implements a model checker for crash consistency on the `ext4` file system.

- `slow` contains a slow implementation.
- `fast` contains a fast implemenation. The two differences from `slow` are
    1. In `lang.rkt`, the `inode-op` structs that make up the DSL are opaque to Rosette to prevent merging
    2. In `ext4.rkt`, the `size` member of the `file` struct is a mutable box, and is accessed using `for/all`.

Each directory contains a `tests` directory with two tests. The `create-rename` test is simpler than the `chrome` test. The slow version of the `chrome` test does not complete in under 5 minutes.