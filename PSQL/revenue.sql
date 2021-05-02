CREATE OR REPLACE FUNCTION revenue() RETURNS NUMERIC(10,2) AS $$
    DECLARE
        orders_desc CURSOR FOR
        SELECT start as day,
               start + interval '1 day' * duration as ending,
               price FROM Orders
            ORDER BY start DESC;
        rec_order RECORD ;
        rec_compatible RECORD ;
        current_revenue NUMERIC(10,2);
        next_revenue NUMERIC(10,2);
        next_compatible_revenue NUMERIC(10,2);
        sentinel DATE;

BEGIN
    DELETE FROM Revenue;
    SELECT INTO sentinel Max(start + interval '1 day' * duration) + interval '1 day' FROM Orders;
    INSERT INTO Revenue (day, ending, amount) VALUES (sentinel, sentinel, 0);
    current_revenue := 0;
    next_revenue := 0;
    OPEN orders_desc;
    LOOP
        FETCH orders_desc INTO rec_order;
        EXIT WHEN NOT FOUND;
        SELECT into rec_compatible day, amount FROM Revenue
            WHERE day = (SELECT Min(day) FROM Revenue WHERE day >= rec_order.ending);
        current_revenue := Greatest(next_revenue, rec_order.price + rec_compatible.amount);
        UPDATE Revenue SET amount = current_revenue WHERE day = rec_order.day;
        IF NOT FOUND THEN
            INSERT INTO Revenue (day, ending, amount)
            VALUES (rec_order.day, rec_order.ending, current_revenue);
        END IF;
        next_revenue := current_revenue;
    END LOOP;
    RETURN current_revenue;
END;
$$ LANGUAGE plpgsql;
