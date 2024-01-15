library(sets)

data=read.csv("wine.txt",header=TRUE,sep=" ")

df <- as.data.frame(data)

set.seed(123)
indices <- sample(1:nrow(data), size = floor(2/3 * nrow(data)), replace = FALSE)
train_data <- data[indices, ]
test_data <- data[-indices, ]

un_alcohol = seq(11, 15, .01)

alcohol_low = fuzzy_cone_gset(center=11, radius=2, universe=un_alcohol)
alcohol_medium = fuzzy_cone_gset(center=13, radius=1, universe=un_alcohol)
alcohol_high = fuzzy_cone_gset(center=15, radius=2, universe=un_alcohol)

un_malic_acid = seq(0.5, 6, .01)

malic_acid_low = fuzzy_cone_gset(center=0.5, radius=2, universe=un_malic_acid)
malic_acid_medium = fuzzy_cone_gset(center=3, radius=4, universe=un_malic_acid)
malic_acid_high = fuzzy_cone_gset(center=6, radius=2, universe=un_malic_acid)

un_color_intensity = seq(1, 12, .01)

color_intensity_low = fuzzy_cone_gset(center=1, radius=4, universe=un_color_intensity)
color_intensity_medium = fuzzy_cone_gset(center=6, radius=3, universe=un_color_intensity)
color_intensity_high = fuzzy_cone_gset(center=12, radius=6, universe=un_color_intensity)

alcohol = fuzzy_variable(alcohol_low = alcohol_low, alcohol_medium = alcohol_medium, alcohol_high = alcohol_high)
malic_acid = fuzzy_variable(malic_acid_low = malic_acid_low, malic_acid_medium = malic_acid_medium, malic_acid_high = malic_acid_high)
color_intensity = fuzzy_variable(color_intensity_low = color_intensity_low, color_intensity_medium = color_intensity_medium, color_intensity_high = color_intensity_high)

plot(alcohol)
plot(malic_acid)
plot(color_intensity)

fuzzy_variables <- set(
  alcohol = alcohol,
  malic_acid = malic_acid,
  color_intensity = color_intensity,
)

get_symbolic_name <- function(value, attribute) {
  if(attribute == 'alcohol') {
    low = attributes(alcohol$alcohol_low[value])$memberships
    medium = attributes(alcohol$alcohol_medium[value])$memberships
    high = attributes(alcohol$alcohol_high[value])$memberships
    
    membership = max(low, medium, high)

    if(identical(membership,low))
    {
      return("alcohol_low")
    }
    else if (identical(membership,medium))
    {
      return("alcohol_medium")
    }
    else if (identical(membership,high))
    {
      return("alcohol_high")
    } 
    else 
    {
      return("alcohol_medium")
    }
  } 
  else if (attribute == 'malic_acid') {
    low = attributes(malic_acid$malic_acid_low[value])$memberships
    medium = attributes(malic_acid$malic_acid_medium[value])$memberships
    high = attributes(malic_acid$malic_acid_high[value])$memberships
    
    membership = max(low, medium, high)
    
    if(identical(membership, low)) {
      return("malic_acid_low")
    } else if (identical(membership, medium)) {
      return("malic_acid_medium")
    } else if (identical(membership, high)) {
      return("malic_acid_high")
    } else 
    {
      return("malic_acid_medium")
    }
  } 
  else if (attribute == 'color_intensity') {
    low = attributes(color_intensity$color_intensity_low[value])$memberships
    medium = attributes(color_intensity$color_intensity_medium[value])$memberships
    high = attributes(color_intensity$color_intensity_high[value])$memberships
    
    membership = max(low, medium, high)
    
    if(identical(membership, low)) {
      return("color_intensity_low")
    } else if (identical(membership, medium)) {
      return("color_intensity_medium")
    } else if (identical(membership, high)) {
      return("color_intensity_high")
    } else 
    {
      return("color_intensity_medium")
    }
    
  }
}

for (attr in c("alcohol", "malic_acid", "color_intensity")) {
  train_data[[attr]] <- sapply(train_data[[attr]], get_symbolic_name, attr = attr)
}

alcohol$alcohol_medium[13.03]

print(alcohol_medium)
alcohol_medium[13.03]
13.03 %in% alcohol_medium

gset_core(alcohol_low)
gset_core(alcohol_medium)
gset_core(alcohol_high)

gset_core(malic_acid_low)
gset_core(malic_acid_medium)
gset_core(malic_acid_high)

gset_core(color_intensity_low)
gset_core(color_intensity_medium)
gset_core(color_intensity_high)

cut(alcohol_low, 0.75)
cut(alcohol_medium, 0.75)
cut(alcohol_high, 0.75)

cut(malic_acid_low, 0.75)
cut(malic_acid_medium, 0.75)
cut(malic_acid_high, 0.75)

cut(color_intensity_low, 0.75)
cut(color_intensity_medium, 0.75)
cut(color_intensity_high, 0.75)

