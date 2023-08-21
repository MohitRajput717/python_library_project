show tables;

select * from agents limit10;
select * from customer limit10;
select * from orders limit10;

-- q1 : share agent name whose order value lesser than 1000rs. -- 

select Agent_name,
od.ord_amount
from agents ag
left join orders as od on od.agent_code= ag.agent_code
where od.ord_amount < 1000.00

-- q2 agent name where belong to india and advance amount < 1000rs --

select ag.agent_name,
cc.cust_country,
od.advance_amount
from agents ag
left join customer as cc on cc.agent_code= ag.agent_code
left join orders as od on od.agent_code = cc.agent_code
where cc.cust_country = 'india'
and od.advance_amount < 1000;


-- q3 country wise total agent count and amount values --

SELECT cust_country, COUNT(cust_country) AS total_customers, SUM(od.ord_amount) AS total_amount
FROM customer cc
LEFT JOIN orders od ON od.agent_code = cc.agent_code
GROUP BY cust_country;



-- q4  agent name commision his country  grade payment outstanding amount phone no  order no and customer code --

select ag.agent_name, ag.commission, ag.phone_no,
cc.cust_country, cc.grade, cc.outstanding_amt,
od.ord_num, od.cust_code
from agents ag
left join customer as cc on cc.agent_code = ag.agent_code
left join orders as od on od.agent_code = cc.agent_code
