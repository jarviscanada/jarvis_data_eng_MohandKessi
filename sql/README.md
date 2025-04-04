# Introduction
This project involves managing a club's membership, facilities, and bookings through an SQL database. 

## Tables

### `cd.membership`

| **Column**           | **Data Type**   | **Description**                      | **Constraints**       |
|----------------------|-----------------|--------------------------------------|-----------------------|
| `id`                 | SERIAL          | Unique identifier for the host       | ⭐ Primary Key        |
| `hostname`           | VARCHAR         | Hostname of the machine              | UNIQUE, NOT NULL       |
| `cpu_number`         | INT2            | Number of CPUs                       | NOT NULL               |
| `cpu_architecture`   | VARCHAR         | CPU architecture (e.g., x86, ARM)    | NOT NULL               |
| `cpu_model`          | VARCHAR         | Model name of the CPU                | NOT NULL               |
| `cpu_mhz`            | FLOAT8          | CPU clock speed in MHz               | NOT NULL               |
| `l2_cache`           | INT4            | Size of L2 cache in KB               | NOT NULL               |
| `timestamp`          | TIMESTAMP       | Time of host data collection         |                       |
| `total_mem`          | INT4            | Total memory                         |                       |

### `cd.facilities`

| **Column**           | **Data Type**   | **Description**                      | **Constraints**       |
|----------------------|-----------------|--------------------------------------|-----------------------|
| `facid`              | INTEGER         | Unique facility identifier           | ⭐ Primary Key        |
| `name`               | VARCHAR(100)    | Name of the facility                 | NOT NULL               |
| `membercost`         | NUMERIC         | Cost for members                     | NOT NULL               |
| `guestcost`          | NUMERIC         | Cost for guests                      | NOT NULL               |
| `initialoutlay`      | NUMERIC         | Initial outlay for the facility      | NOT NULL               |
| `monthlymaintenance` | NUMERIC         | Monthly maintenance cost for the facility | NOT NULL           |

### `cd.bookings`

| **Column**           | **Data Type**   | **Description**                      | **Constraints**       |
|----------------------|-----------------|--------------------------------------|-----------------------|
| `bookid`             | INTEGER         | Unique booking identifier            | ⭐ Primary Key        |
| `facid`              | INTEGER         | ID of the facility booked            | FOREIGN KEY (facid) REFERENCES `cd.facilities`(facid) |
| `memid`              | INTEGER         | ID of the member who made the booking | FOREIGN KEY (memid) REFERENCES `cd.members`(memid) |
| `starttime`          | TIMESTAMP       | Start time of the booking            | NOT NULL               |
| `slots`              | INTEGER         | Number of slots booked               | NOT NULL               |


# SQL Queries

## Table Setup (DDL)

```sql
    CREATE TABLE cd.members
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
            REFERENCES cd.members(memid) ON DELETE SET NULL
    );
    
    CREATE TABLE cd.facilities
    (
       facid INTEGER NOT NULL, 
       name VARCHAR(100) NOT NULL, 
       membercost NUMERIC NOT NULL, 
       guestcost NUMERIC NOT NULL, 
       initialoutlay NUMERIC NOT NULL, 
       monthlymaintenance NUMERIC NOT NULL, 
       CONSTRAINT facilities_pk PRIMARY KEY (facid)
    );
    
    CREATE TABLE cd.bookings
    (
       bookid integer NOT NULL,
       facid integer NOT NULL,
       memid integer NOT NULL,
       starttime timestamp NOT NULL,
       slots integer NOT NULL,
       CONSTRAINT bookings_pk PRIMARY KEY (bookid),
       CONSTRAINT fk_bookings_facid FOREIGN KEY (facid) REFERENCES cd.facilities(facid),
       CONSTRAINT fk_bookins_memid FOREIGN KEY (memid) REFERENCES cd.members(memid),
    )
```

## Questions

### Modifying Data

### Question 1: Insert
```sql
INSERT INTO cd.facilities(facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES(9, 'Spa', 20, 30, 100000, 800)
```
This command adds a new facility, the "Spa," to the `cd.facilities` table with specific costs and maintenance details.

### Question 2: Insert with a Select
```sql
INSERT INTO cd.facilities(facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES((SELECT max(facid) FROM cd.facilities)+1, 'Spa', 20, 30, 100000, 800 )
```
This dynamically assigns the next available `facid` by selecting the current maximum and adding one, then inserts a new facility.

### Question 3: Update (Fix a Mistake)
```sql
UPDATE cd.facilities SET initialoutlay = 10000 WHERE facid = 1;
```
Corrects the initial outlay for the facility with `facid` 1.

### Question 4: Update with Calculation
```sql
UPDATE cd.facilities SET membercost = (SELECT membercost * 1.1 FROM cd.facilities WHERE facid = 0), guestcost = (SELECT guestcost * 1.1 FROM cd.facilities WHERE facid = 0) WHERE facid = 1;
```
Increases the member and guest costs for `facid` 1 by 10%, using the costs of `facid` 0 as a reference.

### Question 5: Delete All
```sql
DELETE FROM cd.bookings;
```
Removes all rows from the `cd.bookings` table.

---

### Basics

### Question 6: Where 2
```sql
SELECT facid, name, membercost, monthlymaintenance FROM cd.facilities WHERE membercost > 0 AND (membercost < monthlymaintenance/50);
```
Fetches facilities where the member cost is greater than 0 but less than 2% of monthly maintenance.

### Question 7: Where 3
```sql
SELECT * FROM cd.facilities WHERE name LIKE '%Tennis%';
```
Retrieves all facilities containing "Tennis" in their name.

### Question 8: Where 4
```sql
SELECT * FROM cd.facilities WHERE facid in (1, 5);
```
Selects facilities with `facid` 1 or 5.

### Question 9: Date
```sql
SELECT memid, surname, firstname, joindate FROM cd.members WHERE joindate >= '2012-09-01';
```
Finds members who joined on or after September 1, 2012.

### Question 10: Union
```sql
SELECT surname FROM cd.members UNION SELECT name FROM cd.facilities;
```
Combines surnames of members and names of facilities, removing duplicates.

---

### Join

### Question 11: Simple Join
```sql
SELECT starttime FROM cd.bookings INNER JOIN cd.members ON cd.members.memid = cd.bookings.memid WHERE cd.members.firstname='David' AND cd.members.surname='Farrell';
```
Finds booking start times for a member named David Farrell.

### Question 12: Simple Join 2
```sql
SELECT cd.bookings.starttime, cd.facilities.name FROM cd.facilities INNER JOIN cd.bookings ON cd.facilities.facid = cd.bookings.facid WHERE cd.facilities.name in ('Tennis Court 2', 'Tennis Court 1') AND cd.bookings.startime>= '2012-09-21' AND cd.bookings.startime < '2012-09-22' ORDER BY cd.bookings.starttime;
```
Lists bookings for specific tennis courts on September 21, 2012, sorted by start time.

### Question 13: Three Joins
```sql
SELECT mems.firstname AS  memfname, mem.surname AS  memsname, recs.firstname AS recfname, recs.surname AS recsname FROM cd.members mems LEFT OUTER JOIN cd.members recs ON recs.memid = mems.recommendedby ORDER BY memsname, memfname;
```
Displays member names and their recommenders, even if there is no recommender.

### Question 14: Three Joins 2
```sql
SELECT DISTINCT recs.firstname AS firstname, recs.surname AS surname FROM cd.members mem INNER JOIN cd.members recs ON recs.memid = mems.recommendedby ORDER BY surname, firstname;
```
Lists unique names of members who recommended others.

### Question 15: Subquery and Join
```sql
SELECT DISTINCT mems.firstname || ' ' || mems.surname AS member, recs.firstname || ' ' || recs.surname AS recommender FROM cd.members mems LEFT JOIN cd.members recs ON recs.memid = mems.recommendedby ORDER BY member;
```
Shows members and their recommenders in a single formatted column.

---

### Aggregation

### Question 16: Group by Order
```sql
SELECT recommendedby, count(*) FROM cd.members WHERE recommendedby IS NOT NULL GROUP BY recommendedby ORDER BY recommendedby;
```
Counts members recommended by each `recommendedby`, ignoring NULL values.

### Question 17: Group By Order 2
```sql
SELECT facid, sum(slots) AS "Total" FROM cd.bookings GROUP BY facid ORDER BY facid;
```
Calculates total slots booked for each facility.

### Question 18: Group By with Condition
```sql
SELECT facid, sum(slots) AS "Total" FROM cd.bookings WHERE starttime>='2012-09-01' AND starttime < '2012-10-01' GROUP BY facid ORDER BY sum(slots);
```
Finds total slots for each facility during September 2012.

### Question 19: Group By Multi-Col
```sql
SELECT facid, DATE_PART('month', starttime) AS month, SUM(slots) AS Total FROM cd.bookings WHERE DATE_PART('year', starttime) = 2012 GROUP BY facid, month ORDER BY facid, month;
```
Groups booking slots by facility and month for 2012.

### Question 20: Count Distinct
```sql
SELECT count(distinct memid) from cd.bookings;
```
Counts distinct member IDs in bookings.

### Question 21: Group By Multiple Cols
```sql
SELECT mems.surname, mems.firstname, mems.memid, MIN(bks.starttime) AS first_booking_time FROM cd.bookings bks INNER JOIN cd.members mems ON mems.memid = bks.memid WHERE bks.starttime >= '2012-09-01' GROUP BY mems.surname, mems.firstname, mems.memid ORDER BY mems.memid;
```
Finds the first booking time for each member after September 1, 2012.

### Question 22: Window Function
```sql
SELECT count(*) OVER(), firstname, surname FROM cd.members ORDER BY joindate;
```
Counts total members while listing each member.

### Question 23: Window Function 2
```sql
SELECT row_number() OVER(ORDER BY joindate), firstname, surname FROM cd.members ORDER BY joindate;
```
Adds a row number to each member based on join date.

### Question 24: Window Function Subquery Group By
```sql
SELECT facid, total FROM (SELECT facid, SUM(slots) AS total, RANK() OVER (ORDER BY SUM(slots) DESC) AS rank FROM cd.bookings GROUP BY facid) AS ranked WHERE rank = 1;
```
Finds the facility with the highest total slots.

---

### String Functions

### Question 25: Format Names of Members
```sql
SELECT surname || ', ' || firstname AS name FROM cd.members;
```
Formats member names as "Last Name, First Name."

### Question 26: WHERE + String Function
```sql
SELECT memid, telephone FROM cd.members WHERE telephone ~ '[()]';
```
Finds members with phone numbers containing parentheses.

### Question 27: Group By Substr
```sql
SELECT SUBSTR(mems.surname, 1,1) AS letter, COUNT(*) AS count FROM cd.members mems GROUP BY letter ORDER BY letter;
```
Groups members by the first letter of their surname and counts them.

