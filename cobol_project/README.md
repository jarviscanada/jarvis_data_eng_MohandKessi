# Student Registration System

A comprehensive COBOL-based system for managing student records, providing functionality for adding, updating, querying, and reporting on student information.

## System Overview

The Student Registration System is designed to efficiently manage student data using VSAM file organization. The system includes programs for:

- Converting sequential files to VSAM format
- Adding new student records
- Updating existing student information
- Deleting student records
- Querying student data through various methods
- Generating reports with course breakdowns

## Programs Included

| Program ID | Description |
|------------|-------------|
| PRGMENU    | Main menu interface that provides access to all system functions |
| PRGV0001   | Converts initial sequential student file into VSAM format |
| PRGI0002   | Adds new student records to the system |
| PRGU0003   | Updates existing student information |
| PRGD0004   | Deletes student records from the system |
| PRGQ0005   | Lists all students in the database |
| PRGQ0006   | Queries student data by student ID |
| PRGQ0007   | Queries student data by date of inclusion |
| PRGR0008   | Generates reports with course breakdowns |

## Data Structure

Each student record contains the following fields:

- Student ID (6 digits, primary key)
- Student Name (30 characters)
- Address (40 characters)
- Phone Number (15 characters)
- Email Address (20 characters)
- Course Code (4 characters)
- Inclusion Date (8 digits, YYYYMMDD format)Sure

## Installation and Setup

1. Compile all programs using a COBOL compiler compatible with your system.
2. Ensure all compiled programs are accessible from the same directory.
3. Prepare an initial sequential file named "STUDENT.DAT" with student records (optional).
4. Run the PRGMENU program to access the system.

```
cobc -x PRGMENU.cbl
cobc -x PRGV0001.cbl
cobc -x PRGI0002.cbl
cobc -x PRGU0003.cbl
cobc -x PRGD0004.cbl
cobc -x PRGQ0005.cbl
cobc -x PRGQ0006.cbl
cobc -x PRGQ0007.cbl
cobc -x PRGR0008.cbl
```

## Usage Guide

### Initial Setup
If starting with a new system, you'll need to either:
1. Create an empty VSAM file (the programs will handle this automatically), or
2. Convert an existing sequential file using option 1 from the main menu (PRGV0001)

### Adding Students
1. Select option 3 from the main menu
2. Enter the requested student information
3. Confirm to save the new record

### Updating Students
1. Select option 4 from the main menu
2. Enter the student ID to update
3. For each field, either enter new data or press Enter to keep the current value
4. Confirm to save the changes

### Querying and Reporting
Various query options are available:
- Option 2: List all students
- Option 6: Find a specific student by ID
- Option 7: List students by inclusion date
- Option 8: Generate reports with course breakdowns

## File Organization

The system uses VSAM (Virtual Storage Access Method) file organization with indexed access, allowing for:
- Fast direct access to records via Student ID
- Sequential processing of all records
- Efficient updating and deletion operations

## Error Handling

The system includes comprehensive error handling for:
- File access errors
- Record not found conditions
- Key violations (duplicate IDs)
- Invalid user input

## Maintenance

Regular backup of the VSAM file is recommended to prevent data loss. The system does not include built-in backup functionality, so this should be handled through external procedures.

## Future Enhancements

Potential improvements to consider:
- User authentication system
- Enhanced search capabilities
- Data export to other formats
- Multi-user support
- Web interface integration

---
