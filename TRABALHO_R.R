#######################################################
#### SCRIPT PARA DEFINIR MELHORES FAULDADES DO PAIS ###
###                                                 ###
###  ARCHIMEDES LAMOTTE - RM30428                   ###
###  ALTAIR LARA - RM                               ###
###  RODRIGO -                                      ###
###                                                 ###
#######################################################


# INSTALACAO E CARGA DE PACOTES NECESARIOS

install.packages("sqldf", dependencies = TRUE) #Instala o pacote
install.packages("gdata", dependencies = TRUE) #Instala o pacote
install.packages("devtools", dependencies = TRUE) #Instala o pacote
require(sqldf)
require(gdata)
require(devtools)
install_github("vqv/ggbiplot")
require(ggbiplot)
require(tcltk)

#MOVE PARA DIRETORIODE TRABALHO (mudar comforme nescessário)
setwd("~/Desktop/MBA/TRABALHO-R-ENADE")

#FUNCAO PARA IMPROMIR CHARTS DO PCA
pcaCharts <- function(x) {
        x.var <- x$sdev ^ 2
        x.pvar <- x.var/sum(x.var)
        print("proportions of variance:")
        print(x.pvar)
        
        par(mfrow=c(2,2))
        plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
        plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
        screeplot(x)
        screeplot(x,type="l")
        par(mfrow=c(1,1))
}

# CARGA BASES CENSO

# CARGA IES
IES<-read.csv2("./Bases/CENSO/DM_IES.csv",sep="|")

#CARGA DOCENTE
DOCENTE<-read.csv2("./Bases/CENSO/DM_DOCENTE.csv",sep="|")

# CALCULA NUMERO DE DOCENTES POR IES POR ESCOLARIDDE
QUERY<-"
Select 
CO_IES,
CASE 
WHEN CO_ESCOLARIDADE_DOCENTE=1 THEN 'Sem graduação'
WHEN CO_ESCOLARIDADE_DOCENTE=2 THEN 'Graduação'
WHEN CO_ESCOLARIDADE_DOCENTE=3 THEN 'Especialização'
WHEN CO_ESCOLARIDADE_DOCENTE=4 THEN 'Mestrado'
WHEN CO_ESCOLARIDADE_DOCENTE=5 THEN 'Doutorado'
ELSE 'Não Informado'
END as ESCOLARIDADE_DOCENTE,
count(*) as NR_DOCENTES 
from DOCENTE 
WHERE 
CO_SITUACAO_DOCENTE=1 
GROUP BY CO_IES,CO_ESCOLARIDADE_DOCENTE"
ESCOLARIDADE_DOCENTE<-sqldf(QUERY,row.names = TRUE)

#CALCULA NUMERO DE MESTRES
QUERY<-"
Select 
CO_IES,
NR_DOCENTES as NR_MESTRES 
from ESCOLARIDADE_DOCENTE 
WHERE 
ESCOLARIDADE_DOCENTE='Mestrado'
"
NUMERO_MESTRES<-sqldf(QUERY,row.names = TRUE)

#CALCULA NUMERO DE DOUTORES
QUERY<-"
Select 
CO_IES,
NR_DOCENTES as NR_DOUTORES 
from ESCOLARIDADE_DOCENTE 
WHERE 
ESCOLARIDADE_DOCENTE='Doutorado'
"
NUMERO_DOUTORES<-sqldf(QUERY,row.names = TRUE)

#CALCULA NUMERO DE ESPECIALISTAS
QUERY<-"
Select 
CO_IES,
NR_DOCENTES as NR_ESPECIALISTAS 
from ESCOLARIDADE_DOCENTE 
WHERE 
ESCOLARIDADE_DOCENTE='Especialização'
"
NUMERO_ESPECIALISTAS<-sqldf(QUERY,row.names = TRUE)

#CALCULA NUMERO DE GRADUADOS
QUERY<-"
Select 
CO_IES,
NR_DOCENTES as NR_GRADUADOS 
from ESCOLARIDADE_DOCENTE 
WHERE 
ESCOLARIDADE_DOCENTE='Graduação'
"
NUMERO_GRADUADOS<-sqldf(QUERY,row.names = TRUE)

#CALCULA NUMERO DE NAO_GRADUADOS
QUERY<-"
Select 
CO_IES,
NR_DOCENTES as NR_NAO_GRADUADOS 
from ESCOLARIDADE_DOCENTE 
WHERE 
ESCOLARIDADE_DOCENTE='Sem graduação'
"
NUMERO_NAO_GRADUADOS<-sqldf(QUERY,row.names = TRUE)

#JUNTA TABELAS DE GRADUACAO E PREENCHE NAs

QUERY<-"
Select 
IES.CO_IES,
CASE 
WHEN D.NR_DOUTORES IS NULL THEN 0
ELSE D.NR_DOUTORES
END AS NR_DOUTORES,
CASE 
WHEN M.NR_MESTRES IS NULL THEN 0
ELSE M.NR_MESTRES
END AS NR_MESTRES,
CASE 
WHEN E.NR_ESPECIALISTAS IS NULL THEN 0
ELSE E.NR_ESPECIALISTAS
END AS NR_ESPECIALISTAS,
CASE 
WHEN G.NR_GRADUADOS IS NULL THEN 0
ELSE G.NR_GRADUADOS
END AS NR_GRADUADOS,
CASE 
WHEN NG.NR_NAO_GRADUADOS IS NULL THEN 0
ELSE NG.NR_NAO_GRADUADOS
END AS NR_NAO_GRADUADOS
From IES IES
LEFT JOIN NUMERO_DOUTORES D on D.CO_IES = IES.CO_IES
LEFT JOIN NUMERO_MESTRES M on M.CO_IES = IES.CO_IES
LEFT JOIN NUMERO_ESPECIALISTAS E on E.CO_IES = IES.CO_IES
LEFT JOIN NUMERO_GRADUADOS G on G.CO_IES = IES.CO_IES
LEFT JOIN NUMERO_NAO_GRADUADOS NG on NG.CO_IES = IES.CO_IES
"
CLASSIFICACAO_DOCENTES<-sqldf(QUERY,row.names = TRUE)

#CARREGA BASE CONCEITO ENADE
CONCEITO_ENADE <- read.xls("./Bases/conceito_enade_2014.xlsx", sheet = 1, header = TRUE,fileEncoding="latin1")

#SEPARA CONCEITO ENADE CONTINUO E POR FAIXA
QUERY<-"
Select 
[Cód..IES] as CO_IES,
AVG([Conceito.Enade..Contínuo.]) as C_ENADE_CONTINUO,
MEDIAN([Conceito.Enade.Faixa.]) as C_ENADE_FAIXA
from CONCEITO_ENADE 
GROUP BY [Cód..IES]"

ENADE_NUMBERS<-sqldf(QUERY, row.names = TRUE)

#CARREGA MICRODADOS ENADE
microdados_enade <- read.table("./Bases/microdados_enade_2014.csv",header=T,sep=";")

#CALCULA MEDIA E MEDIANA DA NOTA GERAL POR IES 
QUERY <- " 
Select 
co_ies as CO_IES,
AVG(nt_ger) as AVG_NOTA_GERAL,
MEDIAN(nt_ger) as MED_NOTA_GERAL
from microdados_enade 
GROUP BY co_ies"

ENADE_NOTAS_ALUNOS <- sqldf(QUERY, row.names = TRUE)

#AGRUPA BASE DE UNIVERCIDADES E PREENCHE NAs 
QUERY <- " 
Select 
CD.CO_IES,
CD.NR_DOUTORES,
CD.NR_MESTRES,
CD.NR_ESPECIALISTAS,
CD.NR_GRADUADOS,
CD.NR_NAO_GRADUADOS,
CASE 
WHEN NA.AVG_NOTA_GERAL IS NULL THEN 0
ELSE NA.AVG_NOTA_GERAL
END AS AVG_NOTA_GERAL,
CASE 
WHEN NA.MED_NOTA_GERAL IS NULL THEN 0
ELSE NA.MED_NOTA_GERAL
END AS MED_NOTA_GERAL,
CASE 
WHEN ED.C_ENADE_CONTINUO IS NULL THEN 0
ELSE ED.C_ENADE_CONTINUO
END AS C_ENADE_CONTINUO,
CASE 
WHEN ED.C_ENADE_FAIXA IS NULL THEN 0
ELSE ED.C_ENADE_FAIXA
END AS C_ENADE_FAIXA
from CLASSIFICACAO_DOCENTES CD
left join ENADE_NUMBERS ED on CD.CO_IES = ED.CO_IES
left join ENADE_NOTAS_ALUNOS NA on CD.CO_IES = NA.CO_IES"

UNIVERSIDADE<-sqldf(QUERY, row.names = TRUE)

#CALCULA PCA

UNIVERSIDADE.pca <- prcomp(UNIVERSIDADE[,2:7],center = TRUE, scale. = TRUE)
print(UNIVERSIDADE.pca)
summary(UNIVERSIDADE.pca)
pcaCharts(UNIVERSIDADE.pca)

#VALORES DE PCA MUITO NEGATIVOS, PROMOVENDO ROTACAO DE EIXO PARA MELHOR VISUALIZACAO
pca.out <- UNIVERSIDADE.pca
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x

#PLOTA DISTRIBUICAO
biplot(pca.out,scale=0, cex=.7)
pca.out$rotation[,1:2]

#PEGA PROPORCOES
pca.var <- pca.out$sdev ^ 2
pca.pvar <- pca.var/sum(pca.var)

#PLOTA DISTRIBUICAO CLASSIFICADA POR FAIXA ENADE
g <- ggbiplot(pca.out, obs.scale = 0, var.scale = 0, labels=row.names(UNIVERSIDADE$CO_IES),groups = UNIVERSIDADE$C_ENADE_FAIXA,
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_continuous(name = 'ENADE FAIXA')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

#CALCULA 5 MELHORES FACULDADES UTILIZANDO AS DUAS VARIAVEIS MAIS RELEVANTES (PC1 E PC2)  

UNIVERSIDADE$PC1 <- pca.out$x[,1]
UNIVERSIDADE$PC2 <- pca.out$x[,2]

UNIVERSIDADE$CLASS <- (pca.pvar[1]*(UNIVERSIDADE$PC1)+pca.pvar[2]*(UNIVERSIDADE$PC2))/(pca.pvar[1]+pca.pvar[2])

TOP5TMP <- head(arrange(UNIVERSIDADE,desc(UNIVERSIDADE$CLASS)), n = 5)

QUERY <- " 
Select 
IE.CO_IES,
IE.NO_IES,
U.CLASS
from IES IE
inner join TOP5TMP U on IE.CO_IES = U.CO_IES
ORDER BY U.CLASS desc"

TOP5<-sqldf(QUERY, row.names = TRUE)

#IMPRIME 5 MELHORS FACULDADES
print(TOP5)


