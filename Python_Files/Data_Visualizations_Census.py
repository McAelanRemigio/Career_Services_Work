"""
Title: Strategic Implications
Author: McAelan Remigio
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import re
from plotly.express import pie as px_pie
from IPython.display import display

df = pd.read_csv("filepath.csv")

# Visualization Functions
def create_pie_chart(df, column_name, title="Pie Chart Responses"):
    vals = df[column_name].dropna()
    vals = vals[vals != ""]
    if vals.empty:
        print(f"No non-empty data in column '{column_name}'.")
        return None
      
    df_counts = (
        vals.value_counts()
        .reset_index()
        .rename(columns={"index": "Choice", column_name: "Count"})
    )
    df_counts["Percent"] = round(100 * df_counts["Count"] / df_counts["Count"].sum(), 1)
    df_counts["Label"] = df_counts.apply(
        lambda x: f"{x['Choice']} - {x['Count']} ({x['Percent']}%)", axis=1
    )

    plt.figure(figsize=(6, 6))
    plt.pie(df_counts["Count"], labels=df_counts["Label"], colors=sns.color_palette("Set3"))
    plt.title(title, fontsize=14, fontweight="bold")
    plt.show()

def create_bar_chart(df, column_name, title="Bar Chart Responses"):
    values = df[column_name].dropna()
    plt.figure(figsize=(8, 5))
    sns.countplot(x=values, color="steelblue")
    plt.title(title)
    plt.xticks(rotation=45, ha="right")
    plt.xlabel(column_name)
    plt.ylabel("Count")
    plt.tight_layout()
    plt.show()

# Example
# create_pie_chart(df, "vars_n", title="Title (optional)")

def update_var_column(df, var_name):
    def categorize(value):
        if pd.isna(value):
            return "Not Set"
        for i in range(1, 7):
            if str(i) in str(value):
                return str(i)
        return "Not Set"

    df[f"updated_{var_name}"] = df[var_name].apply(categorize)
    return df

# Example
# df = update_var_column(df, "var_1")
# create_pie_chart(df, "updated_var_1", title="New Title")

# Text Column + Vector Matching
def count_names(data, text_col, vector_list):
    txt = data[text_col].fillna("").str.lower()
    counts = {name: txt.str.contains(pattern, regex=True).sum() for name, pattern in vector_list.items()}
    name_counts = pd.DataFrame(list(counts.items()), columns=["name", "count"]).sort_values("count", ascending=False)
    display(name_counts)
    return name_counts

# Example vector patterns
vector_list = {
    "Name1": r"\bname1\b",
    "Name2": r"\bname2\b",
    "Name3": r"\bname3\b",
    # ...
    "NameN": r"\bnamen\b"
}

# Example
# df2 = df.rename(columns={"old_feedback_col": "feedback_col"})[["feedback_col"]]
# count_names(df2, "feedback_col", vector_list)

# Filters
def filter_contains(df, column, string):
    return df[df[column].astype(str).str.lower().str.contains(string.lower(), na=False)]

def filter_not_blank(df, column):
    return df[df[column].notna() & (df[column] != "")]

def filter_blank(df, column):
    return df[df[column].isna() | (df[column] == "")]
  
# Example
# df_filter = filter_contains(df, "var_n", "string")
# df_filter_2 = filter_not_blank(df, "Experiential_Learning")

# Subsets
def split_yes_no(df, column):
    df_yes = filter_not_blank(df, column)
    df_no = filter_blank(df, column)
    return df_yes, df_no
  
# Flags
def create_flag(df, name_cols):
    df = df.copy()
    df["has_name_after_flag"] = df[name_cols].notna().any(axis=1)
    df["has_name_after_flag"] = df["has_name_after_flag"].replace({True: "Named", False: "None Named"})
    return df

# Example 
# df_flagged = create_flag(df, ["name_of_one", "name_of_two", "name_of_three"])
# create_pie_chart(df_flagged, "has_name_after_flag", title="Title")
# df_el = filter_not_blank(df, "a_variable")
# create_pie_chart(df_el, "String", title="title")
# df_yes, df_no = split_yes_no(df, "vars")
# create_pie_chart(df_yes, "has_name_after_flag", title="Title")
# create_pie_chart(df_no, "has_name_after_flag", title="Title (Non)")
