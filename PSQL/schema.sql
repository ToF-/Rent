--
-- PostgreSQL database dump
--

-- Dumped from database version 13.2
-- Dumped by pg_dump version 13.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: revenue(); Type: FUNCTION; Schema: public; Owner: tof
--

CREATE FUNCTION public.revenue() RETURNS numeric
    LANGUAGE plpgsql
    AS $$
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
$$;


ALTER FUNCTION public.revenue() OWNER TO tof;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: orders; Type: TABLE; Schema: public; Owner: tof
--

CREATE TABLE public.orders (
    id character varying(8) NOT NULL,
    start date NOT NULL,
    duration integer NOT NULL,
    price numeric(10,2) NOT NULL
);


ALTER TABLE public.orders OWNER TO tof;

--
-- Name: revenue; Type: TABLE; Schema: public; Owner: tof
--

CREATE TABLE public.revenue (
    day date NOT NULL,
    ending date NOT NULL,
    amount numeric(10,2) NOT NULL
);


ALTER TABLE public.revenue OWNER TO tof;

--
-- Name: orders orders_id_key; Type: CONSTRAINT; Schema: public; Owner: tof
--

ALTER TABLE ONLY public.orders
    ADD CONSTRAINT orders_id_key UNIQUE (id);


--
-- Name: revenue revenue_day_key; Type: CONSTRAINT; Schema: public; Owner: tof
--

ALTER TABLE ONLY public.revenue
    ADD CONSTRAINT revenue_day_key UNIQUE (day);


--
-- Name: ix_revenue_day; Type: INDEX; Schema: public; Owner: tof
--

CREATE INDEX ix_revenue_day ON public.revenue USING btree (day);


--
-- PostgreSQL database dump complete
--

