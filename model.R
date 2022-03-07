library(obistools)
library(robis)
library(GeoRange)
library(sp)
library(maps)

#functions
inter <- function(sp1lats, sp1lngs, sp2lats, sp2lngs) {
  ch.sp1 <- cbind(sp1lngs, sp1lats)[chull(sp1lngs, sp1lats),]
  ch.sp2 <- cbind(sp2lngs, sp2lats)[chull(sp2lngs, sp2lats),]
  intersection <- raster::intersect(SpatialPolygons(list(Polygons(list(Polygon(ch.sp1)), 1))),
                                    SpatialPolygons(list(Polygons(list(Polygon(ch.sp2)), 1))))@polygons[[1]]@Polygons[[1]]@coords
  area1 <- CHullAreaEarth(ch.sp1[, 1], ch.sp1[, 2])
  area2 <- CHullAreaEarth(ch.sp2[, 1], ch.sp2[, 2])
  areai <- CHullAreaEarth(intersection[, 1], intersection[, 2])
  print(paste("area 1:", area1))
  print(paste("area 2:", area2))
  print(paste("area of intersection:", areai))
  return(areai / min(area1, area2))
}

#data:
inter.area <- function(sp1lats, sp1lngs, sp2lats, sp2lngs) {
  ch.sp1 <- cbind(sp1lngs, sp1lats)[chull(sp1lngs, sp1lats),]
  ch.sp2 <- cbind(sp2lngs, sp2lats)[chull(sp2lngs, sp2lats),]
  intersection <- raster::intersect(SpatialPolygons(list(Polygons(list(Polygon(ch.sp1)), 1))),
                                    SpatialPolygons(list(Polygons(list(Polygon(ch.sp2)), 1))))@polygons[[1]]@Polygons[[1]]@coords
  return(CHullAreaEarth(intersection[, 1], intersection[, 2]))
}

run <- function(a, b) {
  sp1 <- occurrence(a)
  sp2 <- occurrence(b)

  #plotting
  par(mar = c(1, 1, 1, 1))
  plot(0, type = 'n', xlim = c(-180, 180), ylim = c(-80, 90), xaxt = 'n', yaxt = 'n', bty = 'n', xlab = '', ylab = '')
  map('world', interior = F, add = TRUE, resolution = 0, col = "grey70") #alpha("black",0.3))
  points(sp1$decimalLongitude, sp1$decimalLatitude, pch = 21, bg = "red")
  points(sp2$decimalLongitude, sp2$decimalLatitude, pch = 21, bg = "blue")
  sp1c <- cbind(sp1$decimalLongitude, sp1$decimalLatitude)
  lines(sp1c[c(chull(sp1c), chull(sp1c)[1]),])
  sp2c <- cbind(sp2$decimalLongitude, sp2$decimalLatitude)
  lines(sp2c[c(chull(sp2c), chull(sp2c)[1]),])
  return(inter(sp1$decimalLatitude, sp1$decimalLongitude, sp2$decimalLatitude, sp2$decimalLongitude))
}

run("Colpophyllia natans", "Colpophyllia breviserialis")
run("Pleotrochus zibrowii", "Pleotrochus venustus")
run("Holcotrochus scriptus", "Holcotrochus crenulatus")
run("Thecopsammia elongata", "Thecopsammia socialis")
run("Monomyces pygmaea", "Monomyces rubrum")
run("Deltocyathoides orientalis", "Deltocyathoides stimpsonii")
run("Cyathotrochus pileus", "Cyathotrochus nascornatus")
run("Notocyathus conicus", "Notocyathus venustus") # example
run("Tropidocyathus labidus", "Tropidocyathus lessonii")
run("Dunocyathus wallaceae", "Dunocyathus parasiticus")
run("Trematotrochus hedleyi", "Trematotrochus corbicula")
run("Halomitra clavator", "Halomitra pileus")
run("Heliofungia actiniformi", "Heliofungia fralinae")
run("Dasmosmilia variegata", "Dasmosmilia lymani") # not interpretable
run("Coenosmilia inordinata", "Coenosmilia arbuscula") # question?
run("Dasmosmilia variegata", "Dasmosmilia lymani")
run("Pourtalosmilia conferta", "Pourtalosmilia anthophyllites")
run("Premocyathus dentiformis", "Premocyathus cornuformis")
run("Lochmaeotrochus oculeus", "Lochmaeotrochus gardineri") # discard
run("Rhombopsammia squiresi", "Rhombopsammia niphada")
run("Endopsammia philippensis", "Endopsammia regularis")

run("Heteropsammia cochlea", "Heteropsammia eupsammides") # outliers
run("Enallopsammia profunda", "Enallopsammia pusilla")
run("Cryptotrochus carolinensis", "Cryptotrochus javanus")
run("Idiotrochus alatus", "Idiotrochus emarciatus")
run("Conocyathus formosus", "Conocyathus gracilis")
run("Monohedotrochus capitolii", "Monohedotrochus circularis")
run("Oulangia stokesiana", "Oulangia bradleyi")
run("Ctenactis albitentaculata", "Ctenactis crassa")
run("Cantharellus jebbi", "Cantharellus doederleini")
run("Conotrochus funicolumna", "Conotrochus brunneus") # discard
run("Paraconotrochus antarcticus", "Paraconotrochus zeidleri")
run("Stephanophyllia complicata", "Stephanophyllia neglecta")
run("Leptopenus discus", "Leptopenus hypocoelus")
run("Polymyces fragilis", "Polymyces wellsi") # discard
run("Letepsammia franki", "Letepsammia fissilis") # problematic
run("Eubalaena glacialis", "Eubalaena japonica")
run("Letepsammia formosissima", "Letepsammia superstes")
run("Gardineria minor", "Gardineria simplex")
run("Gardineria paradoxa", "Gardineria hawaiiensis") # problematic
run("Aulocyathus recidivus", "Aulocyathus atlanticus") # problematic
run("Vaughanella margaritata", "Vaughanella concinna") # problematic
run("Vaughanella multipalifera", "Vaughanella oreophila") # strange case
run("Anomocora carinata", "Anomocora fecunda") #p
run("Labyrinthocyathus quaylei", "Labyrinthocyathus facetus") #p
run("Desmophyllum dianthus", "Desmophyllum striatum")
run("Seriatopora hystrix", "Seriatopora dentritica")
run("Seriatopora aculeata", "Seriatopora stellata")
run("Heterocyathus alternatus", "Heterocyathus japonicus")
run("Heterocyathus aequicostatus", "Heterocyathus sulcatus'")
run("Rhizosmilia multipalifera", "Rhizosmilia sagamiensis")
run("Rhizosmilia gerdae", "Rhizosmilia maculata")
run("Phyllangia americana", "Phyllangia pequegnatae")
run("Pleuractis paumotensis", "Pleuractis seychellensis")
run("Lithophyllon concinna", "Lithophyllon repanda")
run("Meandrina danae", "Meandrina brasiliensis")
run("Meandrina jacksoni", "Meandrina meandrites")
run("Colangia jamaicaensis", "Colangia moseleyi")
run("Colangia immersa", "Colangia multipalifera")
run("Cladocora arbuscula", "Cladocora caespitosa") # this one problematic!!!
run("Cladocora debilis", "Cladocora pacifica")
run("Leptastrea bewickensis", "Leptastrea bottae")
run("Leptastrea aequalis", "Leptastrea inaequalis")
run("Blastomussa loyae", "Blastomussa merleti")
run("Tethocyathus cylindraceus", "Tethocyathus recurvatus")
run("Tethocyathus variabilis", "Tethocyathus minor")
run("Tethocyathus virgatus", "Tethocyathus endesa")
run("Mycetophyllia aliciae", "Mycetophyllia lamarckiana")
run("Acanthastrea echinata", "Acanthastrea rotundoflora")
run("Oxypora convoluta", "Oxypora lacera")
run("Echinophyllia patula", "Echinophyllia aspera")
run("Lobophyllia hataii", "Lobophyllia diminuta")

run("Cyphastrea chalcidicum", "Cyphastrea serailia")
run("Cyphastrea japonica", "Cyphastrea decadia")
run("Cyphastrea microphthalma", "Cyphastrea hexasepta")
run("Echinopora lamellosa", "Echinopora ashmorensis")
run("Echinopora horrida", "Echinopora fruticulosa")
run("Echinopora pacifica", "Echinopora gemmacea")
run("Echinopora irregularis", "Echinopora hirsutissima")
run("Echinopora robusta", "Echinopora forskaliana")
run("Hydnophora rigida", "Hydnophora bonsai")
run("Hydnophora microconos", "Hydnophora pilosa")
run("Hydnophora exesa", "Hydnophora grandis")
run("Platygyra crosslandi", "Platygyra yaeyamaensis")
run("Platygyra acuta", "Platygyra ryukyuensis")
run("Favites vasta", "Favites spinosa")
run("Favites halicora", "Favites flexuosa") # example!
run("Goniastrea ramosa", "Goniastrea retiformis")
run("Goniastrea pectinata", "Goniastrea columella")
run("Merulina ampliata", "Merulina scheeri")
run("Fungiacyathus fragilis", "Fungiacyathus stephanus") # not interpretable
run("Foveolocyathus verconis", "Foveolocyathus parkeri")
run("Peponocyathus dawsoni", "Peponocyathus minimus")
run("Platytrochus compressus", "Platytrochus hastatus") #
run("Placotrochides cylindrica", "Placotrochides scaphula") ###
run("Placotrochides frustum", "Placotrochides minuta") ###
run("Rhizotrochus tuberculatus", "Rhizotrochus typus") ###
run("Notophyllia recta", "Notophyllia hecki")
run("Tubastraea diaphana", "Tubastraea coccinea")
run("Tubastraea floreana", "Tubastraea tagusensis")
run("Cladopsammia manuelensis", "Cladopsammia rolandi")
run("Cladopsammia echinata", "Cladopsammia eguchii") # not interpretable
run("Rhizopsammia minuta", "Rhizopsammia nuda")
run("Rhizopsammia pulchra", "Rhizopsammia compacta")
run("Turbinaria radicalis", "urbinaria reniformis") # discard
run("Turbinaria bifrons", "Turbinaria crater") # not interpretable
run("Turbinaria irregularis", "Turbinaria conspicua")
run("Leptopsammia pruvoti", "Leptopsammia queenslandiae")
run("Leptopsammia columna", "Leptopsammia chevalieri")
run("Leptopsammia crassa", "Leptopsammia stokesiana") #??
run("Eguchipsammia cornucopia", "Eguchipsammia wellsi")
run("Eguchipsammia gaditana", "Eguchipsammia serpentina") # example
run("Thalamophyllia gasti", "Thalamophyllia tenuescens")
run("Thalamophyllia gombergi", "Thalamophyllia riisei")
run("Agaricia tenuifolia", "Agaricia agaricites") # question? -- maybe just discard?
run("Agaricia grahamae", "Agaricia undata")
run("Agaricia lamarcki", "Agaricia fragilis") # outliers
run("Pachyseris foliosa", "Pachyseris involuta")
run("Pachyseris speciosa", "Pachyseris gemmae")
run("Euphyllia glabrescens", "Euphyllia paraglabrescens") #example
run("Galaxea acrhelia", "Galaxea longisepta")
run("Galaxea astreata", "Galaxea paucisepta")
run("Isopora cuneata", "Isopora crateriformis") # example
run("Anacropora reticulata", "Anacropora spumosa")
run("Anacropora matthai", "Anacropora spinosa") # discard
run("Anthemiphyllia frustum", "Anthemiphyllia pacifica") # example
run("Anthemiphyllia macrolobata", "Anthemiphyllia multidentata") # example
run("Deltocyathus corrugatus", "Deltocyathus heteroclitus") # need removal
run("Deltocyathus inusitiatus", "Deltocyathus rotulus") # need removal
run("Deltocyathus cameratus", "Deltocyathus nascornatus")
run("Deltocyathus taiwanicus", "Deltocyathus moseleyi")
run("Deltocyathus murrayi", "Deltocyathus halianthus")
run("Deltocyathus philippinensis", "Deltocyathus pourtalesi")
run("Deltocyathus parvulus", "Deltocyathus crassiseptum") # strange case
run("Coenocyathus goreaui", "Coenocyathus caribbeana")
run("Coenocyathus brooki", "Coenocyathus parvulus")
run("Crispatotrochus woodsi", "Crispatotrochus rugosus") # lack of occurrence
run("Crispatotrochus curvatus", "Crispatotrochus septumdentatus")
run("Crispatotrochus irregularis", "Crispatotrochus galapagensis") # lack of occurrence
run("Oculina virgosa", "Oculina valenciennesi")
run("Oculina tenella", "Oculina patagonica") # ?
run("Stylophora madagascarensis", "Stylophora kuehlmanni") #
run("Stylophora danae", "Stylophora pistillata")















