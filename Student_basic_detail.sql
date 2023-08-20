
-- create a new database --
CREATE DATABASE school_db;
-- use databas to insert table--
use school_db;
-- create tables--
CREATE TABLE student (
    CustomerID INT AUTO_INCREMENT PRIMARY KEY,
    FirstName VARCHAR(50),
    LastName VARCHAR(50),
    Email VARCHAR(100),
    mobile int );
 -- change column type--
ALTER TABLE student
MODIFY COLUMN Mobile VARCHAR(20);
   
 -- insert data into table --  
INSERT INTO student (FirstName, LastName, Email, mobile)
VALUES
    ('John', 'Doe', 'john@example.com', '1234567890'),
    ('Jane', 'Smith', 'jane@example.com', '9876543210'),
    ('Alice', 'Johnson', 'alice@example.com', '5555555555'),
    ('Robert', 'Brown', 'robert@example.com', '7777777777'),
    ('Ella', 'Williams', 'ella@example.com', '8888888888'),
    ('Michael', 'Wilson', 'michael@example.com', '9999999999'),
    ('Sophia', 'Miller', 'sophia@example.com', '1111111111'),
    ('Liam', 'Adams', 'liam@example.com', '2222222222');    
   
   
 -- list of student which name not start with J and also last name a --  
select * from student    
where firstName NOT like 'J%' AND  firstName not like '%a';

-- list out only first name and last name --
select FirstName,
LastName
from student;

