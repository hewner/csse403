
use std::future::Future;
use std::pin::pin;
use std::sync::Arc;
use std::task::Context;
use futures::lock::Mutex;
use futures::task::noop_waker_ref;
use futures::join;


// Must acquire the mutex before it can return 2.
async fn returns_two(mutex: Arc<Mutex<()>>) -> i32 {
    println!("  returns_two: trying to acquire mutex...");
    let _guard = mutex.lock().await;
    println!("  returns_two: got the mutex, returning 2");
    2
}

// Returns 3 immediately, no waiting.
async fn returns_three() -> i32 {
    println!("  returns_three: returning 3");
    3
}

// Awaits both results and sums them concurrently.
async fn add_them(mutex: Arc<Mutex<()>>) -> i32 {
    println!("  add_them: getting both futures...");
    let fut_a = returns_two(mutex);
    let fut_b = returns_three();
    println!("  add_them: joining -- both will be polled each time...");
    let (a, b) = join!(fut_a, fut_b);
    println!("  add_them: both resolved ({a} and {b}), returning sum");
    a + b
}

fn main() {
    let mutex = Arc::new(Mutex::new(()));

    // Lock the mutex in main — returns_two will block until we release it.
    let guard = mutex.try_lock().unwrap();
    println!("main: mutex locked");

    let mut cx = Context::from_waker(noop_waker_ref());

    // Pin is required: async state machines can hold self-references,
    // so the future must not move in memory while being polled.
    let mut future = pin!(add_them(mutex.clone()));

    // First poll: returns_two tries to lock the mutex, fails -> Pending
    println!("\nmain: --- first poll ---");
    let result1 = future.as_mut().poll(&mut cx);
    println!("main: first poll returned {:?}", result1);

    // Release the lock, then poll again.
    println!("\nmain: dropping guard (releasing mutex)");
    drop(guard);
    println!("main: --- second poll ---");
    let result2 = future.as_mut().poll(&mut cx);
    println!("main: second poll returned {:?}", result2);
}
