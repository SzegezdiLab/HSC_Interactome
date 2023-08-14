## HSC Interactome

This repository contains the code for the HSC Interactome shiny app for exploring cell-cell interactions between hematopoietic stem cells and other bone marrow cell types. 

The data was generated as part of this study: Single-cell characterisation of the hematopoietic bone marrow interactome in health and myeloid leukemia. *Ennis S et. al., bioRxiv, 2022. DOI: [10.1101/2022.05.13.491790](https://doi.org/10.1101/2022.05.13.491790)*

To play with the shiny app, [click here](https://sarahennis.shinyapps.io/HSC_Interactome/)


---

### Run docker container to host the app locally

The repository contains a Dockerfile and a compose.yaml file to build and run a RStudio server image to host and run the ShinyApp locally. To do so you need [Docker](https://www.docker.com/) and [Docker compose plugin](https://docs.docker.com/compose/) installed. 

Once cloned the repository, go inside the HSC_interactome folder and run:

```
docker compose up
```
It will build and run the container.

With your browser go to:
```
http://localhost:8888/
```

Login in RStudio:

- Name: rstudio
- Password: pass

Once loaded the page, open one of the files (`server.R` or `ui.R`) and click on `Run App` on the top right corner to initialize the app.

