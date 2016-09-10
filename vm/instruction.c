// Call a function
void callFunction(Thread* thread);

// Return from the current function
// If the stack is empty, end the thread
void returnFromFunction(Thread* thread);

// Call a function on a new thread
void startThread(Thread* thread);

// Process a message in the inbox
// If the inbox is empty, defer to another thread
void receive(Thread* thread);

// Send a message to another thread's inbox
void send(Thread* thread);

// Add two numbers
void add(Thread* thread);
