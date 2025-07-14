# User Interface and User Experience Design
# Common Social - Twitter-like Social Media MVP

## 1. Design Philosophy

### 1.1 Core Principles
- **Simplicity First**: Clean, uncluttered interface focusing on content
- **Mobile-First**: Responsive design prioritizing mobile experience
- **Fast Interactions**: HTMX-powered dynamic updates without page reloads
- **Accessibility**: Keyboard navigation and screen reader friendly
- **Minimal Cognitive Load**: Intuitive navigation and clear visual hierarchy

### 1.2 Visual Identity
- **Typography**: System fonts for performance and familiarity
- **Color Palette**: Minimal, high-contrast color scheme
- **Spacing**: Consistent rhythm using Tailwind's spacing scale
- **Icons**: Simple, recognizable iconography (optional SVG icons)

## 2. Layout Structure

### 2.1 Overall Layout
```
┌─────────────────────────────────┐
│           Header                │
├─────────────────────────────────┤
│                                 │
│          Main Content           │
│                                 │
├─────────────────────────────────┤
│           Footer                │
└─────────────────────────────────┘
```

### 2.2 Responsive Breakpoints
- **Mobile**: 320px - 768px
- **Tablet**: 768px - 1024px  
- **Desktop**: 1024px+

## 3. Component Specifications

### 3.1 Header Component
**Purpose**: Site navigation and user status

**Desktop Layout**:
```
┌─────────────────────────────────────────────────────┐
│ [Logo] Common Social    [Compose] [Profile] [Logout] │
└─────────────────────────────────────────────────────┘
```

**Mobile Layout**:
```
┌───────────────────────────┐
│ [☰] Common Social  [+]    │
└───────────────────────────┘
```

**Elements**:
- Logo/Site name (left-aligned)
- Compose button (prominent, right side)
- User profile link (avatar or username)
- Logout option
- Mobile: Hamburger menu for navigation

**Styling**:
- Fixed position at top
- White background with subtle border-bottom
- Shadow for depth
- Height: 60px

### 3.2 Post Composer
**Purpose**: Create new posts

**Layout**:
```
┌─────────────────────────────────────────┐
│ [Avatar] What's happening?              │
│          ┌─────────────────────────────┐ │
│          │                             │ │
│          │ (Text area for post)        │ │
│          │                             │ │
│          └─────────────────────────────┘ │
│          Character count: 280/280       │
│          [Cancel] [Post] ←─ disabled    │
└─────────────────────────────────────────┘
```

**Behavior**:
- Expands on focus from single line to multi-line
- Real-time character counter
- Post button disabled until text entered
- Character counter turns red when approaching limit
- Auto-resize textarea based on content

**HTMX Integration**:
- Form submission without page reload
- Optimistic UI updates
- Error handling with inline messages

### 3.3 Post Display Component
**Purpose**: Display individual posts

**Layout**:
```
┌─────────────────────────────────────────┐
│ [Avatar] @username · 2h                 │
│          Display Name                   │
│                                         │
│ This is the content of the post that    │
│ can span multiple lines and contains    │
│ the user's message.                     │
│                                         │
└─────────────────────────────────────────┘
```

**Elements**:
- User avatar (32x32px on mobile, 48x48px on desktop)
- Username and display name
- Relative timestamp (e.g., "2h", "1d", "Mar 15")
- Post content with proper line breaks
- Subtle border between posts

**Responsive Behavior**:
- Stacked layout on mobile
- Consistent spacing and typography
- Tap targets minimum 44px

### 3.4 Timeline Component
**Purpose**: Display stream of posts

**Layout**:
```
┌─────────────────────────────────────────┐
│ [Compose Post Area]                     │
├─────────────────────────────────────────┤
│ [Post 1]                                │
├─────────────────────────────────────────┤
│ [Post 2]                                │
├─────────────────────────────────────────┤
│ [Post 3]                                │
├─────────────────────────────────────────┤
│ [Load More Posts]                       │
└─────────────────────────────────────────┘
```

**Features**:
- Infinite scroll or "Load More" button
- Smooth loading states
- Empty state messaging
- Pull-to-refresh on mobile (future)

### 3.5 User Profile Component
**Purpose**: Display user information and posts

**Layout**:
```
┌─────────────────────────────────────────┐
│        [Large Avatar]                   │
│        Display Name                     │
│        @username                        │
│        Joined March 2024                │
│        [Edit Profile] (if own profile)  │
├─────────────────────────────────────────┤
│        Posts (timeline format)          │
└─────────────────────────────────────────┘
```

**Elements**:
- Larger avatar (80x80px)
- User information clearly displayed
- Edit profile button for own profile
- Post count and join date
- User's posts in timeline format

## 4. Page Layouts

### 4.1 Home Timeline Page
**URL**: `/`

**Components**:
1. Header
2. Post Composer (logged in users)
3. Timeline of all posts
4. Footer

**Mobile Considerations**:
- Composer accessible via floating action button
- Optimized scroll performance
- Touch-friendly tap targets

### 4.2 User Profile Page
**URL**: `/users/{username}`

**Components**:
1. Header
2. User Profile Info
3. User's Posts Timeline
4. Footer

**States**:
- Own profile (editable)
- Other user's profile (view-only)
- Non-existent user (404 page)

### 4.3 Login Page
**URL**: `/login`

**Layout**:
```
┌─────────────────────────────────────────┐
│                                         │
│            Common Social                │
│                                         │
│        Join the conversation            │
│                                         │
│    ┌─────────────────────────────────┐   │
│    │ Email address                   │   │
│    └─────────────────────────────────┘   │
│                                         │
│         [Send Magic Link]               │
│                                         │
│    Already have an account? Sign in     │
│                                         │
└─────────────────────────────────────────┘
```

**Features**:
- Single email input field
- Clear call-to-action
- Success/error message handling
- Loading states during email sending

### 4.4 Profile Edit Page
**URL**: `/profile/edit`

**Layout**:
```
┌─────────────────────────────────────────┐
│ Edit Profile                            │
├─────────────────────────────────────────┤
│ Display Name:                           │
│ ┌─────────────────────────────────────┐ │
│ │ Current Display Name                │ │
│ └─────────────────────────────────────┘ │
│                                         │
│ Username:                               │
│ ┌─────────────────────────────────────┐ │
│ │ @current_username                   │ │
│ └─────────────────────────────────────┘ │
│                                         │
│ [Cancel]                    [Save]      │
└─────────────────────────────────────────┘
```

## 5. Interactive Elements

### 5.1 HTMX Interactions

**Post Creation**:
```html
<form hx-post="/posts" hx-target="#timeline" hx-swap="afterbegin">
  <!-- Post composer form -->
</form>
```

**Timeline Updates**:
```html
<div hx-get="/api/posts" hx-trigger="load" hx-swap="innerHTML">
  <!-- Timeline content -->
</div>
```

**Character Counter**:
```html
<textarea hx-on:input="updateCharCounter(this)"></textarea>
<span id="char-count">280</span>
```

### 5.2 Loading States
- Skeleton screens for content loading
- Spinner for form submissions
- Disabled states for buttons during actions
- Progress indicators for longer operations

### 5.3 Error States
- Inline validation messages
- Toast notifications for system errors
- Retry mechanisms for failed requests
- Clear error recovery paths

## 6. Accessibility Guidelines

### 6.1 Keyboard Navigation
- Tab order follows logical flow
- All interactive elements keyboard accessible
- Skip links for main content
- Focus indicators clearly visible

### 6.2 Screen Reader Support
- Semantic HTML structure
- ARIA labels where needed
- Alt text for images
- Live regions for dynamic content updates

### 6.3 Color and Contrast
- WCAG AA compliance for text contrast
- Color not sole means of conveying information
- Focus indicators meet contrast requirements

## 7. Tailwind CSS Implementation

### 7.1 Color Palette
```css
/* Primary Colors */
--color-primary: #1d4ed8; /* blue-700 */
--color-primary-hover: #1e40af; /* blue-800 */

/* Neutral Colors */
--color-gray-50: #f9fafb;
--color-gray-100: #f3f4f6;
--color-gray-500: #6b7280;
--color-gray-900: #111827;

/* Status Colors */
--color-red-500: #ef4444; /* errors */
--color-green-500: #10b981; /* success */
```

### 7.2 Typography Scale
```css
/* Headings */
.text-2xl { font-size: 1.5rem; } /* Page titles */
.text-xl { font-size: 1.25rem; } /* Section headers */
.text-lg { font-size: 1.125rem; } /* User names */

/* Body Text */
.text-base { font-size: 1rem; } /* Post content */
.text-sm { font-size: 0.875rem; } /* Meta information */
.text-xs { font-size: 0.75rem; } /* Timestamps */
```

### 7.3 Spacing System
```css
/* Consistent spacing using Tailwind scale */
.space-y-4 > * + * { margin-top: 1rem; } /* Between posts */
.p-4 { padding: 1rem; } /* Card padding */
.mb-6 { margin-bottom: 1.5rem; } /* Section spacing */
```

### 7.4 Component Classes
```css
/* Custom component utilities */
.btn-primary {
  @apply bg-blue-700 text-white px-4 py-2 rounded-md 
         hover:bg-blue-800 focus:ring-2 focus:ring-blue-500
         disabled:opacity-50 disabled:cursor-not-allowed;
}

.card {
  @apply bg-white rounded-lg border border-gray-200 
         shadow-sm hover:shadow-md transition-shadow;
}

.input-field {
  @apply border border-gray-300 rounded-md px-3 py-2 
         focus:ring-2 focus:ring-blue-500 focus:border-blue-500;
}
```

## 8. Performance Considerations

### 8.1 Optimize for Speed
- Minimal CSS and JavaScript bundles
- Lazy loading for images
- Efficient HTMX request patterns
- Optimized font loading

### 8.2 Progressive Enhancement
- Core functionality works without JavaScript
- HTMX enhances but doesn't break basic functionality
- Graceful degradation for older browsers

### 8.3 Mobile Optimization
- Touch-friendly interface elements
- Optimal viewport configuration
- Efficient use of mobile bandwidth
- Fast tap response times

## 9. Testing Strategy

### 9.1 User Testing
- Mobile usability testing
- Accessibility testing with screen readers
- Cross-browser compatibility
- Performance testing on various devices

### 9.2 Automated Testing
- Visual regression testing
- Accessibility audits (axe-core)
- Performance budget monitoring
- HTMX interaction testing