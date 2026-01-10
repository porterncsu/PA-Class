-- macros/generic_tests.sql

{% test no_nulls_in_primary_key(model, column_name) %}
-- Primary keys should never be null
SELECT *
FROM {{ model }}
WHERE {{ column_name }} IS NULL
{% endtest %}


{% test reasonable_percentage(model, column_name, min_pct=0, max_pct=100) %}
-- Values should be valid percentages
SELECT *
FROM {{ model }}
WHERE {{ column_name }} < {{ min_pct }}
   OR {{ column_name }} > {{ max_pct }}
{% endtest %}


{% test no_negative_counts(model, column_name) %}
-- Count fields should not be negative
SELECT *
FROM {{ model }}
WHERE {{ column_name }} < 0
{% endtest %}


{% test reasonable_date_range(model, column_name, earliest_date='1900-01-01', latest_date='2030-12-31') %}
-- Dates should fall within reasonable bounds
SELECT *
FROM {{ model }}
WHERE {{ column_name }} < CAST('{{ earliest_date }}' AS DATE)
   OR {{ column_name }} > CAST('{{ latest_date }}' AS DATE)
{% endtest %}


{% test no_extreme_outliers(model, column_name, std_devs=4) %}
-- Flag values more than N standard deviations from mean
WITH stats AS (
    SELECT 
        AVG({{ column_name }}) as mean_val,
        STDDEV({{ column_name }}) as std_val
    FROM {{ model }}
)
SELECT *
FROM {{ model }}
CROSS JOIN stats
WHERE ABS({{ column_name }} - mean_val) > ({{ std_devs }} * std_val)
{% endtest %}


{% test text_not_empty(model, column_name) %}
-- Text fields shouldn't be empty strings or just whitespace
SELECT *
FROM {{ model }}
WHERE TRIM({{ column_name }}) = ''
   OR {{ column_name }} IS NULL
{% endtest %}


{% test consistent_totals(model, sum_column, part_columns) %}
-- Sum of parts should equal the total
-- Usage: part_columns=['col1', 'col2', 'col3']
SELECT *
FROM {{ model }}
WHERE {{ sum_column }} != (
    {% for col in part_columns %}
        {{ col }} {% if not loop.last %}+{% endif %}
    {% endfor %}
)
{% endtest %}


{% test valid_categorical_values(model, column_name, valid_values) %}
-- Column should only contain specified values
-- Usage: valid_values=['yes', 'no', 'unknown']
SELECT *
FROM {{ model }}
WHERE {{ column_name }} NOT IN (
    {% for value in valid_values %}
        '{{ value }}'{% if not loop.last %},{% endif %}
    {% endfor %}
)
{% endtest %}


{% test no_duplicate_combinations(model, column_names) %}
-- Combination of columns should be unique
-- Usage: column_names=['col1', 'col2']
SELECT 
    {% for col in column_names %}
        {{ col }}{% if not loop.last %},{% endif %}
    {% endfor %},
    COUNT(*) as occurrences
FROM {{ model }}
GROUP BY 
    {% for col in column_names %}
        {{ col }}{% if not loop.last %},{% endif %}
    {% endfor %}
HAVING COUNT(*) > 1
{% endtest %}


{% test reasonable_ratio(model, numerator, denominator, min_ratio=0, max_ratio=1) %}
-- Ratio of two columns should be in expected range
SELECT *
FROM {{ model }}
WHERE {{ denominator }} != 0
  AND ({{ numerator }}::FLOAT / {{ denominator }}::FLOAT < {{ min_ratio }}
   OR {{ numerator }}::FLOAT / {{ denominator }}::FLOAT > {{ max_ratio }})
{% endtest %}