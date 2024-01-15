library(sets)

data=read.csv("wine.txt",header=TRUE,sep=" ")

df <- as.data.frame(data)

set.seed(123)
indices <- sample(1:nrow(data), size = floor(2/3 * nrow(data)), replace = FALSE)
train_data <- data[indices, ]
test_data <- data[-indices, ]

un_alcohol = seq(11, 15, .01)

alcohol_low = fuzzy_cone_gset(center=12, radius=1, universe=un_alcohol)
alcohol_medium = fuzzy_cone_gset(center=13, radius=1, universe=un_alcohol)
alcohol_high = fuzzy_cone_gset(center=14, radius=1, universe=un_alcohol)

un_malic_acid = seq(0.5, 6, .01)

malic_acid_low = fuzzy_cone_gset(center=1, radius=2, universe=un_malic_acid)
malic_acid_medium = fuzzy_cone_gset(center=3, radius=2, universe=un_malic_acid)
malic_acid_high = fuzzy_cone_gset(center=5, radius=2, universe=un_malic_acid)

un_color_intensity = seq(1, 12, .01)

color_intensity_low = fuzzy_cone_gset(center=2, radius=3, universe=un_color_intensity)
color_intensity_medium = fuzzy_cone_gset(center=5, radius=3, universe=un_color_intensity)
color_intensity_high = fuzzy_cone_gset(center=8, radius=3, universe=un_color_intensity)

un_cultivar = seq(1, 3, 1)

cultivar_one = fuzzy_cone_gset(center=1, radius=1, universe=un_cultivar)
cultivar_two = fuzzy_cone_gset(center=2, radius=1, universe=un_cultivar)
cultivar_three = fuzzy_cone_gset(center=3, radius=1, universe=un_cultivar)

alcohol = fuzzy_variable(alcohol_low = alcohol_low, alcohol_medium = alcohol_medium, alcohol_high = alcohol_high)
malic_acid = fuzzy_variable(malic_acid_low = malic_acid_low, malic_acid_medium = malic_acid_medium, malic_acid_high = malic_acid_high)
color_intensity = fuzzy_variable(color_intensity_low = color_intensity_low, color_intensity_medium = color_intensity_medium, color_intensity_high = color_intensity_high)
cultivar = fuzzy_variable(cultivar_one = cultivar_one, cultivar_two = cultivar_two, cultivar_three = cultivar_three)

plot(alcohol)
plot(malic_acid)
plot(color_intensity)
plot(cultivar)

fuzzy_variables <- set(
  alcohol = alcohol,
  malic_acid = malic_acid,
  color_intensity = color_intensity,
  cultivar = cultivar
)

3 %in% malic_acid_high

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
    }
  }
  else if (attribute == 'cultivar') {
    one = attributes(cultivar$cultivar_one[value])$memberships
    two = attributes(cultivar$cultivar_two[value])$memberships
    three = attributes(cultivar$cultivar_three[value])$memberships
    
    membership = max(one, two, three)
    
    if(identical(membership, one)) {
      return("cultivar_one")
    } else if (identical(membership, two)) {
      return("cultivar_two")
    } else if (identical(membership, three)) {
      return("cultivar_three")
    }
  }
}

for (attr in c("alcohol", "malic_acid", "color_intensity", "cultivar")) {
  train_data[[attr]] <- sapply(train_data[[attr]], get_symbolic_name, attr = attr)
}
