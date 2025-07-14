# Product Context: Common Social

## Why This Project Exists
Common Social demonstrates the viability of Common Lisp for modern web development, specifically building a Twitter-like social media platform that rivals mainstream frameworks in functionality and user experience.

## Problems It Solves
1. **Perception Gap**: Shows Common Lisp can build modern, interactive web applications
2. **Development Speed**: Hot loading enables rapid iteration and development
3. **Type Safety**: Comprehensive type checking prevents runtime errors
4. **Performance**: Compiled Lisp code with efficient database operations
5. **Maintainability**: Clear separation of concerns and functional programming patterns

## How It Should Work

### User Experience
- **Seamless Posting**: Users can quickly compose and share thoughts (280 chars)
- **Real-time Interactions**: Likes, follows, and replies update instantly via HTMX
- **Dark Theme**: Modern dark emerald aesthetic that's easy on the eyes
- **Mobile-First**: Responsive design that works well on all devices
- **Fast Navigation**: Single-page app feel with minimal page reloads

### Social Features
- **Profiles**: Rich user profiles with avatars, bios, and follower counts
- **Discovery**: Find and follow interesting users through search and suggestions
- **Engagement**: Like posts, reply to conversations, share content
- **Notifications**: Real-time alerts for social interactions
- **Timeline**: Personalized feed showing posts from followed users

### Developer Experience
- **Hot Loading**: Changes appear instantly without server restarts
- **Type Safety**: Comprehensive validation prevents bugs
- **Functional Architecture**: Clean separation of database, business logic, and presentation
- **REPL Development**: Interactive development and debugging

## User Experience Goals
1. **Familiar**: Feels like Twitter/X but with unique emerald branding
2. **Fast**: Sub-second interactions and page loads
3. **Reliable**: No crashes, proper error handling
4. **Accessible**: Works with screen readers and keyboard navigation
5. **Engaging**: Smooth animations and visual feedback