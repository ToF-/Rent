// rent.h   
#define MAX_ORDERS (10000+1)
#define MAX_START_TIME (2000000)

using namespace std;

class Order {
    public:
    int start_time;
    int duration;
    int amount;
};

class OrderReader {
    public:
    OrderReader(istream &stream) : input (stream) {};
    Order read();
    private:
    istream &input;
};

class RevenueWriter {
    public:
    RevenueWriter(ostream &stream) : output (stream) {};
    void write(int revenue);
    private:
    ostream &output;
};

class Scheduler {
    public:
    Scheduler();
    int get_revenue();
    void add_order(int start, int duration, int bid);

    private:
    int max_cases;
    int max_orders;
    Order next_compatible_order(int k, Order order);
    Order orders[MAX_ORDERS];
};

class Session {
    public:
    Session(istream &input, ostream &output);
    void process();
    private:
    istream &input;
    ostream &output;
};
