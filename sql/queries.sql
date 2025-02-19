-- MODIFYING DATA

--Question 1 : Insert
INSERT INTO cd.facilities(facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES(9, 'Spa', 20, 30, 100000, 800)

--Question 2 : Insert with a select
INSERT INTO cd.facilities(facid, name, membercost, guestcost, initialoutlay, monthlymaintenance) VALUES((SELECT max(facid) FROM cd.facilities)+1, 'Spa', 20, 30, 100000, 800 ))

--Question 3 : Update (fix a mistake)
UPDATE cd.facilities SET initialoutlay = 10000 WHERE facid = 1;

--Question 4 : Update with calculation
UPDATE cd.facilities SET membercost = (SELECT membercost * 1.1 FROM cd.facilities WHERE facid = 0), guestcost = (SELECT guestcost * 1.1 FROM cd.facilities WHERE facid = 0) WHERE facid = 1;

--Question 5 : delete all
DELETE FROM cd.bookings;


-- BASICS

-- Question 6 : Where 2
SELECT facid, name, membercost, monthlymaintenance FROM cd.facilities WHERE membercost > 0 AND (membercost < monthlymaintenance/50);

-- Question 7 : Where 3
SELECT * FROM cd.facilities WHERE name LIKE '%Tennis%';

-- Question 8 : Where 4
SELECT * FROM cd.facilities WHERE facid in (1, 5);

-- Question 9 : Date
SELECT memid, surname, firstname, joindate FROM cd.members WHERE joindate >= '2012-09-01';

-- Question 10 : Union
SELECT surname FROM cd.members UNION SELECT name FROM cd.facilities;


-- JOIN

-- Question 11 : Simple Join
SELECT starttime FROM cd.bookings INNER JOIN cd.members ON cd.members.memid = cd.bookings.memid WHERE cd.members.firstname='David' AND cd.members.surname='Farrell';

-- Question 12 : Simple join 2
SELECT cd.bookings.starttime, cd.facilities.name FROM cd.facilities INNER JOIN cd.bookings ON cd.facilities.facid = cd.bookings.facid WHERE cd.facilities.name in ('Tennis Court 2', 'Tennis Court 1') AND cd.bookings.startime>= '2012-09-21' AND cd.bookings.startime < '2012-09-22' ORDER BY cd.bookings.starttime;

-- Question 13 : Three Joins
SELECT mems.firstname AS  memfname, mem.surname AS  memsname, recs.firstname AS recfname, recs.surname AS recsname FROM cd.members mems LEFT OUTER JOIN cd.members recs ON recs.memid = mems.recommendedby ORDER BY memsname, memfname;

-- Question 14 : Three Joins 2
SELECT DISTINCT recs.firstname AS firstname, recs.surname AS surname FROM cd.members mem INNER JOIN cd.members recs ON recs.memid = mems.recommendedby ORDER BY surname, firstname;

-- Question 15 : Subquery and join
SELECT DISTINCT mems.firstname || ' ' || mems.surname AS member, recs.firstname || ' ' || recs.surname AS recommender FROM cd.members mems LEFT JOIN cd.members recs ON recs.memid = mems.recommendedby ORDER BY member;


-- AGGREGATION

-- Question 16 : Group by Order
SELECT recommendedby, count(*) FROM cd.members WHERE recommendedby IS NOT NULL GROUP BY recommendedby ORDER BY recommendedby;

-- Question 17 : Group By Order 2
SELECT facid, sum(slots) AS "Total" FROM cd.bookings GROUP BY facid ORDER BY facid;

-- Question 18 : Group By with condition
SELECT facid, sum(slots) AS "Total" FROM cd.bookings WHERE starttime>='2012-09-01' AND starttime < '2012-10-01' GROUP BY facid ORDER BY sum(slots);

-- Question 19 : Group By multi col
SELECT facid, DATE_PART('month', starttime) AS month, SUM(slots) AS Total FROM cd.bookings WHERE DATE_PART('year', starttime) = 2012 GROUP BY facid, month ORDER BY facid, month;

-- Question 20 : Count distinct
SELECT count(distinct memid) from cd.bookings;

-- Question 21 : Group by multiple cols 
SELECT mems.surname, mems.firstname, mems.memid, MIN(bks.starttime) AS first_booking_time FROM cd.bookings bks INNER JOIN cd.members mems ON mems.memid = bks.memid WHERE bks.starttime >= '2012-09-01' GROUP BY mems.surname, mems.firstname, mems.memid ORDER BY mems.memid;

-- Question 22 : Window function 
SELECT count(*) OVER(), firstname, surname FROM cd.members ORDER BY joindate;

-- Question 23 : Window function 2
SELECT row_number() OVER(ORDER BY joindate), firstname, surname FROM cd.members ORDER BY joindate

-- Question 24 : Window Function subquery group by
SELECT facid, total FROM (SELECT facid, SUM(slots) AS total, RANK() OVER (ORDER BY SUM(slots) DESC) AS rank FROM cd.bookings GROUP BY facid) AS ranked WHERE rank = 1;


-- STRING

-- Question 25 : Format Names of members
SELECT surname || ', ' || firstname AS name FROM cd.members;       

-- Question 26 : WHERE + String Function
SELECT memid, telephone FROM cd.members WHERE telephone ~ '[()]';

-- Question 27 : Group By substr
SELECT SUBSTR(mems.surname, 1,1) AS letter, COUNT(*) AS count FROM cd.members mems GROUP BY letter ORDER BY letter;