# 📚 Student Batch Management System – COBOL + DB2

## 🎯 Project Overview

This project simulates a student management system using COBOL and DB2. It processes a control file with instructions to insert, update, or delete student records in a DB2 table.

---

## 🗂️ DB2 Table: `STUDENT`

| Field          | Type     | Description                 |
| -------------- | -------- | --------------------------- |
| STUD-ID        | CHAR(4)  | Unique student ID           |
| STUD-NAME      | CHAR(25) | Student's full name         |
| STUD-DOB       | CHAR(8)  | Date of birth (YYYYMMDD)    |
| STUD-COURSE    | CHAR(15) | Course name                 |
| STUD-INS-DATE  | CHAR(8)  | Insert date (YYYYMMDD)      |
| STUD-UPDT-DATE | CHAR(8)  | Last update date (YYYYMMDD) |

---

## 📁 Control File Structure

Each line starts with an operation code:

* `I` → Insert
* `U` → Update
* `D` → Delete

### Example:

```
I0001 JOHN DOE               20010101 COBOL          20240601 00000000
U0002 JANE SMITH             19991212 JAVA           00000000 20240601
D0003
```

---

## 🔧 Programs

### 1. `PRGS0001` – Control File Splitter

Splits the control file into three separate files:

* `INSERT.DAT`
* `UPDATE.DAT`
* `DELETE.DAT`

### 2. `PRGI0002` – Insert Students

* Reads `INSERT.DAT`
* Generates new student IDs
* Inserts new records into DB2

### 3. `PRGS0002` – Statistics Reporter

* Counts inserted, updated, and deleted records
* Writes a summary to a report file

### 4. `PRGU0003` – Update Students

* Reads `UPDATE.DAT`
* Updates matching DB2 records by ID
* Ignores blank fields

### 5. `PRGD0004` – Delete Students

* Reads `DELETE.DAT`
* Deletes records from DB2 by ID

### 6. `PRGS0003` – Error Logger

* Logs error messages to `ERROR.LOG`

### 7. `PRGR0005` – Report All Students

* Lists all students from the DB2 table

### 8. `PRGR0006` – Report by Update Date

* Filters students with update date equal to system date

### 9. `PRGR0007` – Report by Insert Date

* Groups students by insert date

### 10. `PRGR0008` – Report by Course

* Groups students by course

---

## 🧪 Execution Flow

1. Run `PRGS0001` to split the control file
2. Run `PRGD0004`, `PRGI0002`, and `PRGU0003` to process deletions, insertions, and updates
3. Run `PRGS0002` to generate processing stats
4. Run `PRGR0005–PRGR0008` for reports

---

## 📝 Notes

* Each program writes logs to `ERROR.LOG` for error tracking.
* All SQL operations are embedded using `EXEC SQL` blocks.
* Blank fields in `UPDATE.DAT` are ignored (no overwrite).

---

## 👑 Author
Mohand Kessi
