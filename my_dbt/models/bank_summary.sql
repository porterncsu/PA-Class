-- Summarize bank customers by job and marital status
SELECT 
    job,
    marital,
    COUNT(*) as customer_count,
    AVG(age) as avg_age,
    AVG(balance) as avg_balance
FROM {{ ref('bank-full') }}
GROUP BY job, marital
ORDER BY customer_count DESC