# Introduction

# SQL Queries

###### Table Setup (DDL)

    CREATE TABLE members
    (
       memid INTEGER NOT NULL, 
       surname VARCHAR(200) NOT NULL, 
       firstname VARCHAR(200) NOT NULL, 
       address VARCHAR(300) NOT NULL, 
       zipcode INTEGER NOT NULL, 
       telephone VARCHAR(20) NOT NULL, 
       recommendedby INTEGER,
       joindate TIMESTAMP NOT NULL,
       CONSTRAINT members_pk PRIMARY KEY (memid),
       CONSTRAINT fk_members_recommendedby FOREIGN KEY (recommendedby)
            REFERENCES members(memid) ON DELETE SET NULL
    );
    
    CREATE TABLE facilities
    (
       facid INTEGER NOT NULL, 
       name VARCHAR(100) NOT NULL, 
       membercost NUMERIC NOT NULL, 
       guestcost NUMERIC NOT NULL, 
       initialoutlay NUMERIC NOT NULL, 
       monthlymaintenance NUMERIC NOT NULL, 
       CONSTRAINT facilities_pk PRIMARY KEY (facid)
    );
    
    CREATE TABLE bookings
    (
       bookid integer NOT NULL,
       facid integer NOT NULL,
       memid integer NOT NULL,
       starttime timestamp NOT NULL,
       slots integer NOT NULL,
       CONSTRAINT bookings_pk PRIMARY KEY (bookid),
       CONSTRAINT fk_bookings_facid FOREIGN KEY (facid) REFERENCES facilities(facid),
       CONSTRAINT fk_bookins_memid FOREIGN KEY (memid) REFERENCES members(memid),
    )
    
###### Question 1: Insert a new facillity

    INSERT INTO cd.facilities
    (facid, name, membercost, guestcost, initialoutlay, monthlymaintenance)
    VALUES (9, 'Spa', 20, 30, 100000, 800);       

###### Questions 2: Let's try adding the spa to the facilities table again. This time, though, we want to automatically generate the value for the next facid, rather than specifying it as a constant. 




