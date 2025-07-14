# Project Brief: Common Social

## Core Requirements
Build a Twitter-like social media application MVP using Common Lisp, SQLite, HTMX, and Tailwind CSS with the following features:

### Phase 1 (Completed)
- Basic post creation and timeline display
- Character counting and validation (280 chars)
- Real-time interactions with HTMX
- Dark emerald theme with hot loading
- Shift+Enter post submission
- Type-safe Common Lisp implementation

### Phase 2 (In Progress)
- User profiles with display names and bio
- Following/followers system
- Post interactions (likes, replies)
- User discovery and search
- Notifications system
- Enhanced database schema with relationships
- User authentication (magic links)

### Phase 3 (Planned)
- Advanced features like image uploads
- Real-time updates with WebSockets
- Content moderation
- Performance optimization
- Analytics and insights

## Technical Stack
- **Backend**: Common Lisp with SBCL
- **Web Server**: Hunchentoot
- **Database**: SQLite with proper schema
- **Frontend**: HTMX + Tailwind CSS
- **Templating**: CL-WHO (migrated from Djula)
- **Development**: Roswell for Lisp management
- **Features**: Hot loading for development

## Key Constraints
- Must use Common Lisp throughout
- Passwordless magic link authentication
- Mobile-responsive design
- Type safety with comprehensive validation
- Hot loading for rapid development

## Success Criteria
- Functional Twitter-like social interactions
- Clean, responsive UI with dark emerald theme
- Real-time updates without page reloads
- Comprehensive Phase 2 social features working
- Production-ready codebase with proper architecture