# Code chunk is rendered correctly

    Code
      rslt
    Output
      ```{r yml_example}
      yml_empty() %>% yml_author("Malcolm Barrett") %>% yml_output(pdf_document())
      ```

# blanks are correctly cleared from author list

    Code
      author_yml
    Output
      ---
      author:
      - name: John Doe
        affiliation: My Uni
      ---

