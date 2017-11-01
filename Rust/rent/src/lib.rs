
struct Order {
    start_time : i32,
    duration   : i32,
    bid        : i32,
}

impl Order {
    fn compatible(&self, other:Order) -> bool {
        self.start_time >= other.start_time + other.duration
    }
}

fn value(orders: Vec<Order>) -> i32 {
    0
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
    #[test]
    fn an_order_should_not_be_compatible_when_starting_before_end_time() {
        let a = Order { start_time: 0,  duration: 9, bid: 100 }; 
        let b = Order { start_time: 4, duration: 3, bid: 140 }; 
        assert!(!b.compatible(a));
    }
    #[test]
    fn an_order_should_be_compatible_when_starting_at_end_time() {
        let a = Order { start_time: 0,  duration: 9, bid: 100 }; 
        let b = Order { start_time: 9, duration: 3, bid: 140 }; 
        assert!(b.compatible(a));
    }
    #[test]
    fn a_plan_with_zero_orders_is_worth_zero() {
        let p = vec![];
        assert_eq!(0, value(p));
    }
}
