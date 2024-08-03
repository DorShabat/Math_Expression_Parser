import pandas as pd


def convert_lr_table_to_dict(df):
    parsing_table = {}

    for index, row in df.iterrows():
        state = str(row['STATE'])
        parsing_table[state] = {}

        for col in df.columns:
            if col != 'STATE' and not pd.isna(row[col]):
                action = row[col]
                if isinstance(action, float):
                    action = int(action)
                parsing_table[state][col] = action

    return parsing_table


# Load the LR table from the uploaded Excel file
file_path = r"C:\Users\dorsh\Documents\lr_table.xlsx"
lr_table_df = pd.read_excel(file_path, sheet_name='table3')

# Convert the loaded LR table DataFrame into the parsing table dictionary
parsing_table = convert_lr_table_to_dict(lr_table_df)

# Display the resulting parsing table dictionary
print(parsing_table)
print("newwww")
for state, actions in parsing_table.items():
    print(f"'{state}': {actions},")