# Roswell Development Guide

## Shebang for Roswell Scripts

### Correct Shebang Format

When creating executable Lisp scripts with Roswell, use this shebang format:

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- "$0" "$@"
|#
```

### Why This Format?

1. **Cross-platform compatibility**: Works on different Unix-like systems
2. **Proper argument passing**: `"$0" "$@"` ensures all arguments are passed correctly
3. **Quiet mode**: `-Q` flag reduces startup messages
4. **Shell execution**: Uses `/bin/sh` for maximum compatibility

### Common Incorrect Formats

❌ **Don't use:**
```bash
#!/usr/bin/env ros
```

**Problems with simple shebang:**
- May not work on all systems
- Can cause parsing errors
- Doesn't handle arguments properly

### Usage in Common Social

The main server script (`run.lisp`) uses the correct shebang format:

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- "$0" "$@"
|#
;; Common Social - Memory-optimized single-file server

(ql:quickload :hunchentoot :silent t)
;; ... rest of the code
```

### Running Scripts

With the correct shebang, you can run scripts directly:

```bash
# Make executable
chmod +x run.lisp

# Run directly
./run.lisp

# Or via roswell
ros run.lisp
```

### Alternative Loading Methods

If shebang issues persist, you can always load without it:

```bash
# Remove shebang and load
tail -n +6 run.lisp > temp.lisp && ros -e "(load \"temp.lisp\")"

# Or load directly
ros -e "(load \"run.lisp\")"
```

### Roswell Script Best Practices

1. **Always use the proper shebang format**
2. **Load dependencies with `:silent t` to reduce output**
3. **Handle command-line arguments appropriately**
4. **Include error handling for robustness**
5. **Test scripts on different systems when possible**

### Example Complete Script Structure

```lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- "$0" "$@"
|#

;; Load dependencies
(ql:quickload :your-dependencies :silent t)

;; Your code here
(defun main ()
  "Main entry point"
  ;; Your application logic
  )

;; Auto-start
(main)
```

This format ensures your Roswell scripts will run reliably across different environments and installations.