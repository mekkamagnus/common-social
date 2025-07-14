# Tech Context: Common Social

## Core Technologies

### Backend Stack
- **Common Lisp**: SBCL 2.3.10 via Roswell
- **Web Server**: Hunchentoot 1.3.0
- **Database**: SQLite with sqlite package
- **HTML Generation**: CL-WHO (migrated from Djula)
- **Utilities**: Alexandria, local-time, bordeaux-threads
- **Type Checking**: Built-in Common Lisp type system with custom validators

### Frontend Stack
- **Interactivity**: HTMX 1.9.10 for dynamic updates
- **Styling**: Tailwind CSS via CDN
- **Theme**: Dark emerald with hot-swappable themes
- **JavaScript**: Minimal vanilla JS for character counting and keyboard shortcuts

### Development Tools
- **Lisp Manager**: Roswell for implementation management
- **Package Manager**: Quicklisp for dependencies
- **Build System**: ASDF system definitions
- **Hot Loading**: Custom file watching with bordeaux-threads

## Development Setup

### Installation Commands
```bash
# Install Roswell and SBCL
ros install sbcl
ros use sbcl

# Load system
ros -e "(ql:quickload :common-social)"

# Run application
ros run.lisp
```

### Project Structure
```
common-social/
├── common-social.asd          # ASDF system definition
├── package.lisp              # Package definitions
├── run.lisp                  # Startup script
├── enhanced-app.lisp         # Phase 2 standalone app
├── hotload-commands.lisp     # Hot loading commands
├── src/                      # Main source code
│   ├── config.lisp
│   ├── db.lisp
│   ├── models.lisp
│   ├── handlers.lisp
│   ├── main.lisp
│   └── hotload.lisp
├── static/                   # Static assets
├── templates/               # HTML templates (legacy)
└── specs/                   # Documentation
```

## Dependencies

### Required Libraries
```lisp
:depends-on (:hunchentoot      ; HTTP server
             :sqlite           ; Database bindings
             :cl-who           ; HTML generation
             :local-time       ; Time handling
             :bordeaux-threads ; Threading
             :split-sequence   ; String utilities
             :alexandria)      ; General utilities
```

### Development Dependencies
- **Testing**: FiveAM or Rove (not yet implemented)
- **Documentation**: Built-in docstrings and CLAUDE.md

## Technical Constraints

### Performance
- SQLite suitable for MVP, may need PostgreSQL for scale
- Memory usage optimized with type declarations
- Hot loading adds development overhead but worth it

### Compatibility
- Requires SBCL (other Lisps not tested)
- Modern browsers for HTMX and Tailwind
- macOS/Linux development environment

### Security
- Input validation through type system
- SQL injection prevention via parameterized queries
- Session management for authentication (planned)

## Tool Usage Patterns

### REPL Development
```lisp
;; Interactive testing
(start-server)
(create-user "test@example.com" "testuser" "Test User")
(get-posts 10)
```

### Hot Loading Workflow
1. Edit `hotload-commands.lisp`
2. Changes apply within 2 seconds
3. Browser automatically updates
4. No server restart needed

### Database Management
```lisp
;; Schema updates
(init-db)
(create-default-user)

;; Query testing
(get-posts 20)
(get-user-by-username "demo")
```

## Current Issues

### Syntax Problems
- Enhanced-app.lisp has parentheses balance issues in render-posts function
- Phase 2 features implemented but not deployable due to syntax errors
- Original simple app was working on port 8008

### Missing Features
- User authentication system (magic links)
- Image upload functionality
- Real-time WebSocket updates
- Comprehensive error handling

### Technical Debt
- Large HTML rendering functions need refactoring
- Test suite not implemented
- Documentation could be more comprehensive
- Production deployment strategy undefined