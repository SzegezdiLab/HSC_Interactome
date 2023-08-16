## HSC Interactome

This repository contains the code for the HSC Interactome shiny app for exploring cell-cell interactions between hematopoietic stem cells and other bone marrow cell types. 

The data was generated as part of this study: Cell-cell interactome of the hematopoietic niche and its changes in acute myeloid leukemia. *Ennis S et. al., iScience, 2023. DOI: [10.1016/j.isci.2023.106943](https://doi.org/10.1016/j.isci.2023.106943)*

To play with the shiny app, [click here](https://sarahennis.shinyapps.io/HSC_Interactome/)

OR

You can download this repository and run the app locally by following the instructions below...


---

### Run docker container to host the app locally

The repository contains a Dockerfile and a compose.yaml file to build and run a RStudio server image to host and run the ShinyApp locally. To do so you need [Docker](https://www.docker.com/) and [Docker compose plugin](https://docs.docker.com/compose/) installed. 

Once the repository has been cloned, go inside the HSC_interactome folder and run:

```bash
docker compose up
```
It will build and run the container.

With your browser go to: [http://localhost:8888/](http://localhost:8888/)

Login to RStudio:

- Name: rstudio
- Password: pass

Once the page has loaded, open one of the files (`server.R` or `ui.R`) and click on `Run App` on the top right corner to initialize the app.

If you have any questions or problems running the app, feel free to raise an issue [here](https://github.com/SzegezdiLab/HSC_Interactome/issues).