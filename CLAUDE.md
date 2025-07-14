# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Twitter-like social media application MVP built with Common Lisp, SQLite, HTMX, and Tailwind CSS. The application uses Roswell for Common Lisp implementation management and features magic link authentication for passwordless login.

## Common Lisp Development Commands

### Roswell Commands
```bash
# Install and manage Common Lisp implementations
ros install sbcl          # Install SBCL
ros use sbcl              # Switch to SBCL
ros run                   # Start REPL with current implementation

# Load system using Roswell
ros -e "(ql:quickload :common-social)"

# Run Lisp code directly
ros -e "(asdf:load-system :common-social)"
```

### ASDF System Management
```bash
# Load the system via Roswell
ros -e "(asdf:load-system :common-social)"

# Load system with Quicklisp via Roswell
ros -e "(ql:quickload :common-social)"
```

### Testing
```bash
# Run tests using Roswell
ros -e "(asdf:test-system :common-social)"

# Run tests and exit
ros -e "(progn (asdf:test-system :common-social) (uiop:quit))"

# Common test frameworks in Common Lisp:
# - FiveAM: ros -e "(progn (ql:quickload :common-social) (fiveam:run!))"
# - Prove: ros -e "(prove:run :common-social-test)"
# - Rove: ros -e "(rove:run :common-social-test)"
```

### Development Environment
```bash
# Start REPL using Roswell
ros run

# Load and start the application
ros -e "(push (uiop:getcwd) asdf:*central-registry*)" -e "(asdf:load-system :common-social)" -e "(common-social:start-server)"

# Or run the startup script
ros run.lisp
```

## Project Structure Conventions

### Project Structure for Twitter-like MVP
- `src/` - Main application code
  - `web/` - Web server and routing
  - `auth/` - Magic link authentication system
  - `posts/` - Tweet/post functionality
  - `users/` - User management
  - `db/` - Database schema and operations
- `static/` - Static assets (CSS, JS, images)
- `templates/` - HTML templates
- `tests/` - Test files
- `migrations/` - Database migration scripts
- `*.asd` - ASDF system definition files
- `package.lisp` - Package definitions

### Key Application Components
- Web server using Hunchentoot or similar
- SQLite database with appropriate schema
- HTMX for dynamic frontend interactions
- Tailwind CSS for styling
- Magic link email authentication
- Basic Twitter-like features (posts, timeline, user profiles)

## Development Guidelines

### ASDF System Definition
- Define systems in `.asd` files with proper dependencies
- Use `:depends-on` for external libraries
- Specify `:components` for source file organization
- Include test system definitions

### Package Management
- Define packages in dedicated package files
- Use descriptive package names with appropriate prefixes
- Export symbols intentionally from packages
- Consider package-local nicknames for convenience

### Required Libraries for This Project
Essential libraries for the Twitter-like social app:
- **Web Server**: Hunchentoot (HTTP server)
- **Database**: sqlite (SQLite bindings for Common Lisp)
- **Templating**: CL-WHO (HTML generation)
- **JSON**: Jonathan or Shasht (API responses)
- **Email**: cl-smtp (for magic link emails)
- **Utilities**: Alexandria, local-time
- **Testing**: FiveAM or Rove

### Frontend Stack
- **HTMX**: For dynamic interactions without full page reloads
- **Tailwind CSS**: Utility-first CSS framework for styling
- **Alpine.js** (optional): For additional client-side interactivity

### MVP Features to Implement
1. **User Authentication**
   - Magic link email-based login
   - Session management
   - User registration/profile creation

2. **Core Social Features**
   - Create and publish posts (tweets)
   - View timeline of posts
   - User profiles with post history
   - Basic following/followers (optional for MVP)

3. **Database Schema**
   - Users table (id, email, username, created_at)
   - Posts table (id, user_id, content, created_at)
   - Sessions table (id, user_id, token, expires_at)
   - Magic_links table (id, email, token, expires_at)

### Interactive Development
- Use the REPL for interactive development and testing
- Leverage `(trace function-name)` for debugging
- Use `(describe object)` and `(inspect object)` for introspection
- Take advantage of condition system for error handling
- Test HTMX endpoints directly via REPL

## Coding Philosophy
- Always use functional programming patterns when possible

## Development Workflow
- Use a Test-Driven Development workflow. Test should be created before implementation.

## Type Checking
- Use common lisp type checking to improve code reliability and catch potential errors early