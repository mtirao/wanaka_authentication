--
-- PostgreSQL database dump
--

-- Dumped from database version 15.3
-- Dumped by pg_dump version 17.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: tenants; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tenants (
    user_name text NOT NULL,
    user_password text NOT NULL,
    user_id text NOT NULL,
    status text,
    created_at bigint
);


ALTER TABLE public.tenants OWNER TO postgres;

--
-- Data for Name: tenants; Type: TABLE DATA; Schema: public; Owner: postgres
--

INSERT INTO public.tenants VALUES
	('marcos.tirao@icloud.com', '3172AppL', '92525840', 'enabled', 1),
	('marcos.tirao', '3172AppL', 'mtirao@gmail.com', 'enabled', 1);


--
-- Name: tenants tenants_user_id_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tenants
    ADD CONSTRAINT tenants_user_id_key UNIQUE (user_id);


--
-- PostgreSQL database dump complete
--

