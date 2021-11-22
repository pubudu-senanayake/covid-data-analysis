
# Setup script for folders -----------------------------------------------

if (!dir.exists("data")) {
  dir.create("data")
} else {
  print("Directory named data already exists, and does not need to be created")
}

if (!dir.exists("out")) {
  dir.create("out")
} else {
  print("Directory named out already exists, and does not need to be created")
}

if (!dir.exists("plots")) {
  dir.create("plots")
} else {
  print("Directory named plots already exists, and does not need to be created")
}
