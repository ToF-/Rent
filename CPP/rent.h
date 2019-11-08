// rent.h   
#define MAX_ORDERS (10000+1)
#define MAX_START_TIME (2000000)

class Order {
    public:
    int start_time;
    int duration;
    int amount;
};

class Scheduler {
    public:
    int get_revenue();
    void add_order(int start, int duration, int bid);


    Scheduler();
    private:
    int max_orders;
    Order next_compatible_order(int k, int end_time);
    Order orders[MAX_ORDERS];
};
