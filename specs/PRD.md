# Product Requirements Document (PRD)
# Common Social - Twitter-like Social Media MVP

## 1. Product Overview

### 1.1 Vision
Create a minimal viable product (MVP) for a Twitter-like social media platform that enables users to share short-form content and engage with others in a simple, fast, and secure manner. This is a **mobile-first application** designed primarily for mobile users with desktop as a secondary consideration.

### 1.2 Goals
- **Primary**: Launch a functional social media platform with core posting and reading capabilities
- **Secondary**: Implement passwordless authentication for improved user experience
- **Technical**: Demonstrate Common Lisp viability for modern web applications

### 1.3 Success Metrics
- Users can successfully register and authenticate via magic links
- Users can create, view, and interact with posts
- Platform loads quickly with HTMX-driven interactions
- System handles at least 100 concurrent users

## 2. User Stories

### 2.1 Authentication
- **As a new user**, I want to sign up with just my email address so I can quickly join the platform
- **As a returning user**, I want to log in via a magic link sent to my email so I don't need to remember a password
- **As a user**, I want to stay logged in across browser sessions so I don't need to re-authenticate frequently

### 2.2 Content Creation
- **As a user**, I want to compose and publish posts up to 280 characters so I can share my thoughts
- **As a user**, I want to see a real-time character counter while typing so I know how much space I have left
- **As a user**, I want my posts to appear immediately in my timeline after publishing

### 2.3 Content Consumption
- **As a user**, I want to see a chronological timeline of all posts so I can read what others are sharing
- **As a user**, I want to view individual user profiles and their post history
- **As a user**, I want posts to load dynamically without full page refreshes for a smooth experience

### 2.4 User Management
- **As a user**, I want to set a display name and username for my profile
- **As a user**, I want to view my own profile and post history
- **As a user**, I want to edit my profile information

## 3. Functional Requirements

### 3.1 User Authentication System
- **Magic Link Authentication**
  - Email-based registration and login
  - Secure token generation with expiration (15 minutes)
  - Session management with secure cookies
  - Automatic account creation on first magic link use

- **Session Management**
  - Persistent sessions across browser restarts
  - Secure session tokens
  - Session timeout after 30 days of inactivity
  - Logout functionality

### 3.2 User Management
- **Profile System**
  - Unique username (3-20 characters, alphanumeric + underscore)
  - Display name (1-50 characters)
  - Email address (for authentication)
  - Account creation timestamp
  - Profile editing capabilities

### 3.3 Content System
- **Post Creation**
  - Character limit: 280 characters
  - Real-time character counter
  - Timestamp on creation
  - Immediate publication (no drafts in MVP)

- **Post Display**
  - Chronological timeline view (newest first)
  - Individual post permalinks
  - User profile pages with post history
  - Post metadata (author, timestamp)

### 3.4 User Interface
- **Mobile-First Responsive Design**
  - Primary design and development focus on mobile devices (320px-768px)
  - Touch-optimized interface with minimum 44px tap targets
  - Thumb-friendly navigation and interaction patterns
  - Desktop experience as progressive enhancement
  - Clean, minimal interface using Tailwind CSS
  - Fast interactions powered by HTMX

- **Core Pages (Mobile-Optimized)**
  - Home timeline with mobile-optimized scrolling
  - User profiles with mobile-friendly layouts
  - Login/registration page optimized for mobile keyboards
  - Post composition interface with mobile text input optimization

## 4. Technical Requirements

### 4.1 Technology Stack
- **Backend**: Common Lisp with Hunchentoot web server
- **Database**: SQLite for data persistence
- **Frontend**: HTMX for dynamic interactions, Tailwind CSS for styling
- **Email**: SMTP integration for magic link delivery
- **Infrastructure**: Single server deployment initially

### 4.2 Database Schema

#### Users Table
```sql
users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  email TEXT UNIQUE NOT NULL,
  username TEXT UNIQUE NOT NULL,
  display_name TEXT NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
)
```

#### Posts Table
```sql
posts (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL,
  content TEXT NOT NULL CHECK(length(content) <= 280),
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id)
)
```

#### Sessions Table
```sql
sessions (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL,
  token TEXT UNIQUE NOT NULL,
  expires_at DATETIME NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (user_id) REFERENCES users(id)
)
```

#### Magic Links Table
```sql
magic_links (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  email TEXT NOT NULL,
  token TEXT UNIQUE NOT NULL,
  expires_at DATETIME NOT NULL,
  used BOOLEAN DEFAULT FALSE,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
)
```

### 4.3 API Endpoints

#### Authentication
- `POST /auth/request-magic-link` - Request magic link via email
- `GET /auth/verify/{token}` - Verify magic link and create session
- `POST /auth/logout` - End user session

#### Posts
- `GET /` - Home timeline
- `POST /posts` - Create new post
- `GET /posts/{id}` - View individual post
- `GET /users/{username}` - User profile with posts

#### Users
- `GET /profile` - Current user profile
- `POST /profile` - Update user profile

### 4.4 Performance Requirements
- Page load times under 2 seconds on mobile networks (3G+)
- HTMX interactions complete within 500ms on mobile devices
- Support for 100 concurrent users
- Database queries optimized with appropriate indexes
- Mobile-optimized asset delivery and caching
- Minimal JavaScript bundle size for mobile performance

## 5. Non-Functional Requirements

### 5.1 Security
- HTTPS enforcement
- Secure session token generation
- SQL injection prevention
- XSS protection
- Rate limiting on magic link requests (max 3 per email per hour)

### 5.2 Usability
- Intuitive mobile-first navigation with thumb-friendly design
- Clear visual feedback for user actions on touch devices
- Accessible design following basic WCAG guidelines
- Mobile-responsive interface with touch-optimized interactions
- One-handed operation support for mobile users
- Optimized for various mobile screen sizes and orientations

### 5.3 Reliability
- 99% uptime target
- Graceful error handling
- Data backup strategy
- Input validation and sanitization

## 6. Constraints and Assumptions

### 6.1 Constraints
- MVP scope limited to core functionality only
- Single-server deployment
- SQLite database (no complex scaling initially)
- No real-time features (WebSockets) in MVP
- No image/media uploads in MVP

### 6.2 Assumptions
- Users have email access for magic link authentication
- Primary user access via mobile devices with modern mobile browsers
- Users comfortable with mobile-first interface patterns
- Initial user base will be small (< 1000 users)
- SMTP service available for email delivery
- Mobile users have sufficient data/wifi for web app usage

## 7. Future Considerations (Post-MVP)

### 7.1 Features
- Following/followers system
- Like and reply functionality
- Real-time notifications
- Image and media uploads
- Search functionality
- Direct messaging

### 7.2 Technical Improvements
- Database migration to PostgreSQL
- Caching layer (Redis)
- CDN for static assets
- Horizontal scaling capabilities
- Advanced monitoring and analytics

## 8. Success Criteria

### 8.1 Launch Criteria
- All core user stories implemented and tested
- Magic link authentication working reliably on mobile devices
- Timeline displays posts correctly with mobile-optimized scrolling
- Users can create accounts and post content via mobile interface
- Mobile-first responsive design fully functional across device sizes
- Touch interactions and mobile usability validated

### 8.2 Post-Launch Metrics
- User registration conversion rate > 60%
- Daily active users
- Posts per user per day
- System uptime and performance metrics
- User retention rate (7-day and 30-day)

## 9. Timeline and Milestones

### Phase 1: Foundation (Week 1-2)
- Project setup and basic Common Lisp application
- Database schema implementation
- Basic web server and routing

### Phase 2: Authentication (Week 3)
- Magic link system implementation
- Session management
- Basic user registration flow

### Phase 3: Core Features (Week 4-5)
- Post creation and display
- Timeline implementation
- User profiles

### Phase 4: Polish and Launch (Week 6)
- HTMX integration for dynamic interactions
- Tailwind CSS styling
- Testing and bug fixes
- Deployment preparation