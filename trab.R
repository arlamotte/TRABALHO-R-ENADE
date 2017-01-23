rm(DOCENTE)
DOCENTE[10,]
ESCOLARIDADE_DOCENTE<-sqldf("
           Select 
                CO_IES,
                CASE 
                        WHEN CO_ESCOLARIDADE_DOCENTE=1 THEN 'Sem graduação'
                        WHEN CO_ESCOLARIDADE_DOCENTE=2 THEN 'Graduação'
                        WHEN CO_ESCOLARIDADE_DOCENTE=3 THEN 'Especialização'
                        WHEN CO_ESCOLARIDADE_DOCENTE=4 THEN 'Mestrado'
                        WHEN CO_ESCOLARIDADE_DOCENTE=4 THEN 'Doutorado'
                        ELSE 'Não Informado'
                END as ESCOLARIDADE_DOCENTE,
                count(*) as NR_DOCENTES 
           from DOCENTE 
           WHERE 
                CO_SITUACAO_DOCENTE=1 
           GROUP BY CO_IES,CO_ESCOLARIDADE_DOCENTE", 
           row.names = TRUE)
rm(TMP)


CONCEITO_ENAD <- read.xls("../conceito_enade_2014.xlsx", sheet = 1, header = TRUE,fileEncoding="latin1")

CONCEITO_ENADE

tmp<-sqldf("
           Select 
      [Cód..IES] as CD_IES,
      AVG([Conceito.Enade..Contínuo.]) as C_ENAD_CONTINUO,
      MEDIAN([Conceito.Enade.Faixa.]) as C_ENAD_FAIXA
      from CONCEITO_ENADE 
      GROUP BY [Cód..IES]", 
      row.names = TRUE)

CONCEITO_ENADE
head(UNIVERSIDADE)
log.tmp <- log(UNIVERSIDADE[,2:8])
tmp.pca <- prcomp(UNIVERSIDADE[,2:8],center = TRUE, scale. = TRUE)
tmp.pca
plot(tmp.pca, type = "l")
plot(tmp.pca, type = "b")

g <- ggbiplot(tmp.pca, obs.scale = 1, var.scale = 1, groups = UNIVERSIDADE$C_ENADE_FAIXA, ellipse = TRUE, cricle=TRUE )
g <- g + scale_colour_continuous(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

rm(QUERY)
QUERY<-"
Select 
CO_IES,
CASE 
WHEN ESCOLARIDADE_DOCENTE='Mestrado' THEN SUM(NR_DOCENTES) END as NR_MESTRES,
CASE 
WHEN ESCOLARIDADE_DOCENTE='Doutorado' THEN SUM(NR_DOCENTES) END as NR_DOUTORES,
CASE 
WHEN ESCOLARIDADE_DOCENTE='Especialização' THEN NR_DOCENTES END as NR_ESPECIALISTAS,
CASE 
WHEN ESCOLARIDADE_DOCENTE='Graduação' THEN NR_DOCENTES END as NR_GRADUADOS,
CASE 
WHEN ESCOLARIDADE_DOCENTE='Sem graduação' THEN NR_DOCENTES END as NR_NAO_GRADUADOS
FROM ESCOLARIDADE_DOCENTE 
GROUP BY CO_IES"
CLASSIFICACAO_DOCENTES<-sqldf(QUERY,row.names = TRUE)

row.names(UNIVERSIDADE)
pc1 <- pca.out$rotation[,1]
test <-data.frame(t(pc1))

test$NR_DOUTORES


UNIVERSIDADE$CALSS <-(UNIVERSIDADE$NR_DOUTORES*test$NR_DOUTORES+UNIVERSIDADE$NR_MESTRES*test$NR_MESTRES+UNIVERSIDADE$NR_ESPECIALISTAS*test$NR_ESPECIALISTAS+UNIVERSIDADE$NR_GRADUADOS*test$NR_GRADUADOS+UNIVERSIDADE$NR_NAO_GRADUADOS*test$NR_NAO_GRADUADOS+UNIVERSIDADE$AVG_NOTA_GERAL*test$AVG_NOTA_GERAL)/sum(test[1,])
        
UNIVERSIDADE$CALSS <-(UNIVERSIDADE$NR_DOUTORES*6+UNIVERSIDADE$NR_MESTRES*5+UNIVERSIDADE$NR_ESPECIALISTAS*4+UNIVERSIDADE$NR_GRADUADOS*3+UNIVERSIDADE$NR_NAO_GRADUADOS*2+UNIVERSIDADE$AVG_NOTA_GERAL*1)/16
        sum(test[1,])
sum(test[1,])

newdata <- UNIVERSIDADE[order(-UNIVERSIDADE$CALSS),]
head(arrange(UNIVERSIDADE,desc(UNIVERSIDADE$CALSS)), n = 5)
test2 <- merge(x = newdata, y = IES, by = "CO_IES", all = TRUE)
