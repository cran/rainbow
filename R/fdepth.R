fdepth <- function(data, type = c("FM", "mode", "RP", "RPD"), trim = 0.25)
{
  type = match.arg(type)
  if (type == "FM"){
      output <- depth.FM(data, trim = trim)
  }
  else if (type == "mode"){
      output <- depth.mode(data, trim = trim)
  }
  else if (type == "RP"){
      output <- depth.RP(data, trim = trim)
  }
  else output <- depth.RPD(data, trim = trim)
  return(structure(list(data=data, output=output), class="fdepth"))
}

