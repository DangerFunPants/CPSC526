// Overall execution should be as follows:
//  * Each sim iteration represents one second of real time.
//  * The following will need to happen in each sim loop:
//      1. Increment pending time for each issued ticket.
//      2. Decrement wait time to next ticket acquisition for
//         every user.
//      3. Select new random service for users whose ticket request wait
//         time has expired.

// Data structures
// User:
//  * List of current tickets
//  * Wait time until next ticket acquisition.
//
// Ticket:
//  * Timer until expiration.

extern crate rand;
use rand::distributions::{Distribution, Exp};
use rand::Rng;

#[derive(Debug)]
struct User {
    current_tickets: Vec<Ticket>,
    request_timer: u64,
}

impl User {
    fn new() -> Self {
        User {
            current_tickets: Vec::new(),
            request_timer: 0,
        }
    }

    fn decrequest_timer(&mut self) {
        if self.request_timer > 0 {
            self.request_timer = self.request_timer - 1;
        }
    }

    fn set_wait(&mut self, t: u64) {
        self.request_timer = t;
    }

    fn has_ticket_for_service(&mut self, service: u32) -> bool {
        for i in 0..self.current_tickets.len() {
            if self.current_tickets[i].service_id == service
                && self.current_tickets[i].valid_count > 0 {
                return true;
            }
        }
        false
    }

    fn add_new_ticket(&mut self, t: Ticket) {
        self.current_tickets.push(t);
    }
}

// These are called attributes in Rust. The derive attribute lets you
// inherit default behaviour for some traits.
// Rust trait ~= Haskell Typeclass
#[derive(Debug)]
struct Ticket {
    valid_count: u64,
    service_id: u32,
}

impl Ticket {
    fn new(valid_count: u64, service_id: u32) -> Self {
        Ticket {
            valid_count: valid_count,
            service_id: service_id,
        }
    }

    fn dec_valid_timer(&mut self) {
        self.valid_count = self.valid_count - 1;
    }
}

fn get_wait_time() -> u64 {
    let exp = Exp::new(0.25);
    let wait_time = exp.sample(&mut rand::thread_rng());
    (wait_time * 60.0) as u64
}

fn select_service(service_count: u32) -> u32 {
    // not sure why you need the type here?
    let services: Vec<u32> = (0..service_count).collect();
    // choose will return none if an empty list
    // is passed in.
    match rand::thread_rng().choose(&services) {
        Some(v) => *v,
        None => 0,
    }
}

// Vec::iter() returns an iterator over the vector, while Iter::collect()
// returns a vector containing the elements over which the iterator
// is iterating.

fn create_user_list(user_count: u32) -> Vec<User> {
    (0..user_count).map(|_| User::new()).collect()
}

fn update_sim_state(user_list: &mut Vec<User>, service_count: u32) {
    for i in 0..user_list.len() {
        for j in 0..user_list[i].current_tickets.len() {
            if user_list[i].current_tickets[j].valid_count > 0 {
                user_list[i].current_tickets[j].dec_valid_timer();
            }
        }

        if user_list[i].request_timer == 0 {
            let service = select_service(service_count);
            // println!("Selecting service: {}", service);
            if !user_list[i].has_ticket_for_service(service) {
                let new_ticket = Ticket::new(30 * 60, service);
                user_list[i].add_new_ticket(new_ticket);
            }
            let wait_time = get_wait_time();
            user_list[i].set_wait(wait_time);
        }
        user_list[i].decrequest_timer();
    }
}

fn calculate_avg_tickets_per_user(user_list: &Vec<User>) -> f64 {
    let total = calculate_total_tickets(&user_list);
    (total as f64) / (user_list.len() as f64)
}

fn calculate_total_tickets(user_list: &Vec<User>) -> u32 {
    user_list
        .iter()
        .fold(0usize, |sum, v| sum + v.current_tickets.len()) as u32
}

fn print_sim_state(user_list: &Vec<User>, service_count: u32) {
    let avg = calculate_avg_tickets_per_user(&user_list);
    let total = calculate_total_tickets(&user_list);
    println!(
        "Results for simulation with {} users and {} services",
        user_list.len(),
        service_count
    );
    println!("Average tickets per user: {}", avg);
    println!("Total number of tickets requested: {}", total);
}

fn run_sim_loop(mut user_list: Vec<User>, service_count: u32, sim_time: u64) -> Vec<User> {
    for _ in 0..sim_time {
        update_sim_state(&mut user_list, service_count);
    }
    user_list
}

fn main() {
    let user_list = create_user_list(100);
    let result = run_sim_loop(user_list, 10, 900 * 4 * 8);
    print_sim_state(&result, 10);
}
