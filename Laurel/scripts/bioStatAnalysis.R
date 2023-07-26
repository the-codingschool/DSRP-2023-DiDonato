## data set of all biological factors
bio_factors <- select(health_overview, id, high_blood_pressure, gender,
                      high_cholesterol, depressed, anemia, overweight,
                      sedentary_minutes, sleep_hours, bmi, depressed)

