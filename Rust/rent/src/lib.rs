
struct Order {
    start_time : i32,
    duration   : i32,
    bid        : i32,
}

impl Order {
    fn compatible(&self, other:Order) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_order_should_be_compatible_when_starting_after() {
        let a = Order { start_time: 0,  duration: 9, bid: 100 }; 
        let b = Order { start_time: 10, duration: 3, bid: 140 }; 
        assert!(b.compatible(a));
    }
}
