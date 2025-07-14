# Common Social

> A Twitter-like social media application MVP demonstrating Common Lisp's viability for modern web development

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Common Lisp](https://img.shields.io/badge/Language-Common%20Lisp-brightgreen.svg)](https://common-lisp.net/)
[![SBCL](https://img.shields.io/badge/Lisp-SBCL-blue.svg)](http://www.sbcl.org/)

A modern social media platform built with Common Lisp, SQLite, HTMX, and Tailwind CSS, featuring mobile-first design, hot loading development, and type-safe architecture.

## 🚀 Features

### Phase 1 (Complete) ✅
- **Single-user posting system**: Create posts up to 280 characters with real-time validation
- **Timeline display**: Chronological post timeline with mobile-optimized scrolling
- **HTMX interactions**: Dynamic updates without page reloads
- **Mobile-first UI**: Touch-optimized interface with Tailwind CSS
- **Hot loading development**: Live code updates with zero downtime
- **Dark emerald theme**: Modern aesthetic with theme switching capability

### Phase 2 (In Development) 🔧
- **User profiles**: Display names, bios, and avatar support
- **Social interactions**: Following/followers system and post likes
- **Magic link authentication**: Passwordless login via email
- **Notifications**: Real-time alerts for social interactions
- **Enhanced database schema**: Complete relational design with constraints

### Phase 3 (Planned) 📋
- **Advanced features**: Image uploads, hashtags, mentions
- **Real-time updates**: WebSocket integration
- **Content moderation**: Reporting and admin tools
- **Performance optimization**: Caching and scaling improvements

## 🏁 Quick Start

### Prerequisites
- [Roswell](https://github.com/roswell/roswell) (Common Lisp implementation manager)
- SBCL (installed via Roswell)
- Git

### Installation

1. **Install Roswell and SBCL**:
   ```bash
   # macOS (via Homebrew)
   brew install roswell
   
   # Or install manually from: https://github.com/roswell/roswell
   ros install sbcl
   ros use sbcl
   ```

2. **Clone and run**:
   ```bash
   git clone https://github.com/your-username/common-social.git
   cd common-social
   ros run.lisp
   ```

3. **Alternative: Interactive development**:
   ```bash
   ros -e "(ql:quickload :common-social)"
   ros -e "(common-social:start-server)"
   ```

4. **Visit the application**:
   Open [http://localhost:8008](http://localhost:8008) in your browser

### Hot Loading Development

For rapid development with live code updates:

```bash
# Start the server
ros run.lisp

# In another terminal, edit hotload-commands.lisp
# Changes apply automatically within 2 seconds
```

## 💻 Development

### REPL Development
```lisp
;; Load and start the system
(ql:quickload :common-social)
(common-social:start-server)

;; Server management
(common-social:stop-server)
(common-social:restart-server)

;; Hot loading new features
(load "hotload-commands.lisp")
```

### SLY/SLIME Integration

For advanced interactive development:

```bash
# Start with SLY support
ros run --eval "(ql:quickload :common-social)" --eval "(common-social:start-server)"

# Connect from Emacs: M-x sly-connect
# Live code evaluation, debugging, and hot loading
```

### Development Workflow

1. **File-based hot loading**: Edit `hotload-commands.lisp` for automatic updates
2. **REPL development**: Interactive function modification and testing
3. **Live debugging**: Fix issues while server runs and users browse
4. **Type safety**: Comprehensive validation prevents runtime errors

## 📁 Project Structure

```
common-social/
├── 📄 common-social.asd        # ASDF system definition
├── 📄 package.lisp             # Package definitions
├── 📄 run.lisp                 # Startup script
├── 📄 hotload-commands.lisp    # Hot loading commands
├── 📁 src/                     # Source code
│   ├── config.lisp            # Configuration
│   ├── db.lisp                # Database layer
│   ├── models.lisp            # Data models
│   ├── handlers.lisp          # Web handlers
│   ├── utils.lisp             # Utilities
│   ├── main.lisp              # Main application
│   └── hotload.lisp           # Hot loading system
├── 📁 specs/                   # Project documentation
│   ├── PRD.md                 # Product requirements
│   ├── TECH.md                # Technical specifications
│   └── UI.md                  # UI/UX guidelines
├── 📁 ai-docs/                 # Development guides
│   └── common-lisp.md         # Common Lisp development guide
├── 📁 commands/                # Context and memory bank
├── 📁 static/                  # Static assets (CSS, JS)
├── 📁 templates/               # HTML templates (legacy)
└── 📁 enhanced-app.lisp        # Phase 2 application (in development)
```

## ⚡ Technology Stack

### Backend
- **Language**: Common Lisp (SBCL)
- **Web Server**: Hunchentoot
- **Database**: SQLite with full schema
- **Templates**: CL-WHO (HTML generation)
- **Type System**: Comprehensive validation

### Frontend
- **Interactivity**: HTMX 1.9.10
- **Styling**: Tailwind CSS (CDN)
- **Theme**: Dark emerald with hot swapping
- **JavaScript**: Minimal vanilla JS

### Development
- **Package Manager**: Quicklisp
- **Build System**: ASDF
- **Hot Loading**: Custom file watching system
- **IDE Integration**: SLY/SLIME support

## 🗄️ Database Schema

### Phase 1 (Current)
```sql
CREATE TABLE posts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    content TEXT NOT NULL CHECK(length(content) <= 280),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

### Phase 2 (Enhanced Schema)
```sql
-- Users table
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    email TEXT UNIQUE NOT NULL,
    username TEXT UNIQUE NOT NULL,
    display_name TEXT NOT NULL,
    bio TEXT,
    avatar_url TEXT,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Posts with user association
CREATE TABLE posts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    content TEXT NOT NULL CHECK(length(content) <= 280),
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Social features
CREATE TABLE likes (/*...*/);
CREATE TABLE follows (/*...*/);
CREATE TABLE notifications (/*...*/);
```

Database file: `common-social.db` (created automatically)

## 📱 Mobile-First Design

- **Touch-optimized**: 44px minimum tap targets
- **One-handed operation**: Thumb-friendly navigation
- **Responsive breakpoints**: 320px, 768px, 1024px+
- **Auto-resizing**: Smart text area expansion
- **Smooth interactions**: HTMX-powered updates
- **Dark theme**: Easy on the eyes, modern aesthetic

## 🚀 Live Coding Features

### Hot Loading System
- **File watching**: Automatic reload of `hotload-commands.lisp`
- **Zero downtime**: Update code while server runs
- **Theme switching**: Live CSS and layout changes
- **Type safety**: Comprehensive validation prevents errors

### Interactive Development
- **REPL integration**: Live function evaluation
- **SLY support**: Advanced debugging and introspection
- **State preservation**: Keep user sessions during updates
- **Error recovery**: Interactive condition system

## 🤝 Contributing

### Development Guidelines
1. **Functional patterns**: Pure functions where possible
2. **Type safety**: Use comprehensive validation
3. **Mobile-first**: Design for touch interfaces
4. **Hot loading**: Test changes with live reloading
5. **Documentation**: Update relevant docs and examples

### Getting Started
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes using hot loading for rapid iteration
4. Test on mobile devices and various screen sizes
5. Commit your changes (`git commit -m 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## 📚 Documentation

- [`CLAUDE.md`](./CLAUDE.md) - Project instructions for AI development
- [`specs/PRD.md`](./specs/PRD.md) - Product requirements document
- [`specs/TECH.md`](./specs/TECH.md) - Technical specifications
- [`ai-docs/common-lisp.md`](./ai-docs/common-lisp.md) - Common Lisp development guide
- [`commands/`](./commands/) - Context and memory bank files

## 🏆 Why Common Lisp?

This project demonstrates that Common Lisp is viable for modern web development:

- **Live coding**: Unparalleled development experience with hot loading
- **Type safety**: Comprehensive validation catches errors early
- **Performance**: Compiled code with efficient memory management
- **Stability**: Mature language with decades of production use
- **Interactivity**: REPL-driven development for rapid iteration

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details

---

**Built with ❤️ and Common Lisp**