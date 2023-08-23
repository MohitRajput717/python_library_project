-- need agent wise all amount total with country wise --
select cc.cust_country, COUNT(*) AS total_customers,
sum(cc.OPENING_AMT),
sum(cc.RECEIVE_AMT),
sum(cc.PAYMENT_AMT),
sum(cc.outstanding_amt),
sum(od.ORD_AMOUNT),
sum(od.ADVANCE_AMOUNT)
from customer cc
left join agents as ag on ag.AGENT_CODE = cc.AGENT_CODE
left join orders as od on od.AGENT_CODE = ag.AGENT_CODE
group by 1;

-- differnce between opening and recieve amount -- 
select cc.cust_country, COUNT(*) AS total_customers,
sum(cc.OPENING_AMT)- sum(cc.RECEIVE_AMT) as differnce_amout

from customer cc
left join agents as ag on ag.AGENT_CODE = cc.AGENT_CODE
left join orders as od on od.AGENT_CODE = ag.AGENT_CODE
group by 1;




-- only require postive amounts -- 

select cc.cust_country, COUNT(*) AS total_customers,
sum(cc.OPENING_AMT)- sum(cc.RECEIVE_AMT) as differnce_amout
from customer cc
left join agents as ag on ag.AGENT_CODE = cc.AGENT_CODE
left join orders as od on od.AGENT_CODE = ag.AGENT_CODE
group by 1
having differnce_amout > 0;

-- if require only negetive amount --


select cc.cust_country, COUNT(*) AS total_customers,
sum(cc.OPENING_AMT)- sum(cc.RECEIVE_AMT) as differnce_amout
from customer cc
left join agents as ag on ag.AGENT_CODE = cc.AGENT_CODE
left join orders as od on od.AGENT_CODE = ag.AGENT_CODE
group by 1
having differnce_amout < 0;