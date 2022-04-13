# argument types are respected in code_chunk()

    Code
      cat(chunk1)
    Output
      ```{r fig.cap = "something"}
      plot(1:5)
      ```

---

    Code
      cat(chunk2)
    Output
      ```{r fig.cap = label_object}
      plot(1:5)
      ```

---

    Code
      cat(chunk3)
    Output
      ```{r fig.height = 3}
      plot(1:5)
      ```

