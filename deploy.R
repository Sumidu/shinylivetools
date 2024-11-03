# deploys all apps from the apps folder using shinylive in the _site directory

# get list of apps
applist <- dir("apps")

sources <- paste0("apps/", applist)
targets <- paste0("_site/",applist)
target <- "_site"

for (app in 1:length(applist)) {
  message(paste0("Deploying ",applist[app], "..."))
  shinylive::export(sources[app], target, subdir = applist[app])
}

#shinylive::export("apps/", "_site")
rmarkdown::render("index.Rmd",output_dir = "_site")

# test
# httpuv::runStaticServer("_site/regextester/")

# print folder size of _site without system call
files<-list.files("_site", full.names = TRUE, recursive = TRUE)
vect_size <- sapply(files, file.size)
size_files <- sum(vect_size)
message(paste("Output folder size:",utils:::format.object_size(size_files, "auto")))
