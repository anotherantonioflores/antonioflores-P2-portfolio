# processed-data

This folder contains data that has been processed and cleaned by code.

Any files located in here are based on the raw data and can be re-created running the various processing/cleaning code scripts in the `code` folder.

1. Processeddata1.rds is the original clean data set with both character-factor predictors and numeric predictors
2. Processeddata2.rds is a data set that attempted to utilize Dummy Variables. I did not end up using this for analysis
3. Processeddata3.rds is the clean data set with the character factors converted to numeric factors.
4. Processeddata4.rds is the same as processeddata3 except the numeric factors have been converted to true numeric values
5. Processeddata5.rds is the same as processedata4 except additional "attribute" information has been stripped from the numeric categorical values for the SOLE purpose of working with the Corrplot in the EDA QMD.
