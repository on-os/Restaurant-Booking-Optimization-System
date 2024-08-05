# Restaurant Booking Optimization System
This repository contains the solution to the assignment of a System for Optimizing Restaurant Bookings as part of the MSc Prolog course (2022-2023). The project implements an AI system to help a small bistro in Brussels optimize its evening service and take bookings online through natural language input. 

## Features
- **Definite Clause Grammar (DCG):** Parses natural language input for restaurant bookings.
- **Constraint Logic Programming (CLP(FD)):** Manages and optimizes bookings based on restaurant constraints.
- **Natural Language Processing (NLP):** Understands and processes booking requests in English.

## How to Use
1. Ensure you have SWI-Prolog installed.
2. Clone this repository.
3. In order to run the project, test cases are included in the `restaurant.pl` file.

## Example Input
Here are some example booking requests in natural language and their corresponding Prolog list format:
**Natural Language:**
_- "Table for 2 at 20:00 on 18 March."
- "Please can we have a table for 3 for the theatre menu on March 18th?"
- "We would like a table for 5 preferably at 8pm on 18/03."_

**Prolog List Format:**
_- [table,for,2,at,20,':',00,on,18,march]
- [please,can,we,have,a,table,for,3,for,the,theatre,menu,on,march,18,th]
- [we,would,like,a,table,for,5,preferably,at,8,pm,on,18,'/',03]_

## Acknowledgments
The assignment was part of the MSc Prolog course, and the ideas were drawn from the course materials on DCGs, CLP(FD), and constraint meta-programming.
