# deploys all apps from the apps folder using shinylive in the _site directory

# get list of apps
applist <- dir("apps")

sources <- paste0("apps/", applist)
targets <- paste0("_site/",applist)

for (app in 1:length(applist)) {
  print(app)
  shinylive::export(sources[app], targets[app])
}

#shinylive::export("apps/", "_site")