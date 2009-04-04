--
-- PostgreSQL database dump
--

SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

--
-- Name: s_seq_forum_id; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE s_seq_forum_id
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: s_forums; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE s_forums (
    forum_id integer DEFAULT nextval('s_seq_forum_id'::regclass) NOT NULL,
    parent_id integer,
    forum_name text,
    forum_desc text
);


--
-- Name: s_seq_rev_id; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE s_seq_rev_id
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: s_post_revisions; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE s_post_revisions (
    rev_id integer DEFAULT nextval('s_seq_rev_id'::regclass) NOT NULL,
    post_id integer,
    rev_updated timestamp without time zone,
    rev_author integer,
    rev_author_ip character varying(16),
    post_subject text,
    post_body text,
    post_img_id integer
);


--
-- Name: s_seq_post_id; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE s_seq_post_id
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: s_posts; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE s_posts (
    post_id integer DEFAULT nextval('s_seq_post_id'::regclass) NOT NULL,
    parent_id integer,
    forum_id integer,
    author_id integer,
    post_created timestamp without time zone,
    rev_id integer
);


--
-- Name: s_seq_user_id; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE s_seq_user_id
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: s_users; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE s_users (
    user_id integer DEFAULT nextval('s_seq_user_id'::regclass) NOT NULL,
    user_name character varying(255),
    user_displayname character varying(255),
    user_password character varying(128),
    user_created timestamp without time zone,
    user_lastvisit timestamp without time zone
);


--
-- Name: s_forums_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY s_forums
    ADD CONSTRAINT s_forums_pkey PRIMARY KEY (forum_id);


--
-- Name: s_post_revisions_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY s_post_revisions
    ADD CONSTRAINT s_post_revisions_pkey PRIMARY KEY (rev_id);


--
-- Name: s_posts_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY s_posts
    ADD CONSTRAINT s_posts_pkey PRIMARY KEY (post_id);


--
-- Name: s_users_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY s_users
    ADD CONSTRAINT s_users_pkey PRIMARY KEY (user_id);


--
-- Name: s_forums_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY s_forums
    ADD CONSTRAINT s_forums_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES s_forums(forum_id);


--
-- Name: s_post_revisions_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY s_post_revisions
    ADD CONSTRAINT s_post_revisions_post_id_fkey FOREIGN KEY (post_id) REFERENCES s_posts(post_id);


--
-- Name: s_post_revisions_rev_author_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY s_post_revisions
    ADD CONSTRAINT s_post_revisions_rev_author_fkey FOREIGN KEY (rev_author) REFERENCES s_users(user_id);


--
-- Name: s_posts_author_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY s_posts
    ADD CONSTRAINT s_posts_author_id_fkey FOREIGN KEY (author_id) REFERENCES s_users(user_id);


--
-- Name: s_posts_forum_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY s_posts
    ADD CONSTRAINT s_posts_forum_id_fkey FOREIGN KEY (forum_id) REFERENCES s_forums(forum_id);


--
-- Name: s_posts_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY s_posts
    ADD CONSTRAINT s_posts_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES s_posts(post_id);


--
-- Name: s_posts_rev_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY s_posts
    ADD CONSTRAINT s_posts_rev_id_fkey FOREIGN KEY (rev_id) REFERENCES s_post_revisions(rev_id);


--
-- PostgreSQL database dump complete
--

