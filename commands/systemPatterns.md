# System Patterns: Common Social

## Architecture Overview
The application follows a clean separation of concerns with functional programming principles:

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Web Browser   │◄──►│  Hunchentoot     │◄──►│    SQLite       │
│  (HTMX/Tailwind)│    │  (Common Lisp)   │    │   Database      │
└─────────────────┘    └──────────────────┘    └─────────────────┘
                              │
                        ┌──────────────────┐
                        │   Hot Loading    │
                        │     System       │
                        └──────────────────┘
```

## Key Technical Decisions

### 1. Template Migration: Djula → CL-WHO
- **Rationale**: Better integration with Lisp, compile-time checking
- **Pattern**: HTML generation through S-expressions
- **Benefit**: Type safety and hot reloading compatibility

### 2. Database Design
- **Relational Schema**: Users, Posts, Likes, Follows, Notifications
- **Foreign Keys**: Proper referential integrity
- **Indexes**: Performance optimization on common queries
- **Type Constraints**: Database-level validation

### 3. Type Safety System
```lisp
(deftype post-content ()
  '(and string (satisfies valid-post-content-p)))

(defun validate-and-assert (value type error-message)
  (unless (typep value type)
    (error "Type validation failed: ~A" error-message))
  value)
```

### 4. Hot Loading Architecture
- **File Watching**: Background thread monitors hotload-commands.lisp
- **Dynamic Updates**: Theme and functionality changes without restart
- **Zero Downtime**: Server continues running during updates

## Component Relationships

### Core Modules
1. **Database Layer** (`src/db.lisp`, `src/models.lisp`)
   - Connection management
   - CRUD operations with type checking
   - Schema migrations

2. **Web Layer** (`src/handlers.lisp`, `src/main.lisp`)
   - HTTP request handling
   - Route definitions
   - Response generation

3. **Presentation Layer** (CL-WHO templates)
   - HTML generation
   - Theme system
   - Component reusability

4. **Hot Loading** (`src/hotload.lisp`)
   - File monitoring
   - Dynamic reloading
   - Development assistance

### Phase 2 Extensions
- **User Management**: Profile creation, authentication
- **Social Features**: Following, likes, notifications
- **Enhanced UI**: User avatars, interaction buttons
- **Real-time Updates**: HTMX-powered interactions

## Critical Implementation Paths

### Request Flow
1. Browser → HTMX request → Hunchentoot
2. Route dispatch → Handler function
3. Database query with type validation
4. CL-WHO template rendering
5. HTML response → Browser DOM update

### Hot Loading Flow
1. Developer edits hotload-commands.lisp
2. File watcher detects change
3. Background thread loads new code
4. Theme/functionality updates instantly
5. Browser reflects changes automatically

## Design Patterns
- **Functional Core**: Pure functions for business logic
- **Imperative Shell**: Side effects isolated to boundaries
- **Type-First Design**: Comprehensive validation throughout
- **Component Composition**: Reusable UI building blocks
- **Event-Driven Updates**: HTMX triggers and responses