#Primeiro é necessário instalar os pacotes, isto só deve ser feito uma vez na máquina de uso. 
#Remova o '#' das linhas de instalação caso seja necessário instalar os pacotes pela primeira vez na máquina.
#%%
#pip install numpy 
#pip install pandas
#pip install python-Levenshtein

#%% Chamando as bibliotecas dos pacotes instalados acima
import numpy as np
import pandas as pd
import fuzzywuzzy as fw
from rapidfuzz import process, utils
from datetime import datetime, date

#%% Importando os dados
#Importando somente as variáveis que serão utilizadas para deixar o processamento mais leve
colunas = ['Record_Id','nomecompletoPaciente', 'dataNascimentoFull','mae', 'sexo', 'cpf', 'dataPrimeiroSintomas', 'testeRT_PCR', 'Reinfecção', 'Laboratorio']
dados = pd.read_csv('DadosQualidadeCovid19.csv',sep=";", usecols=colunas, encoding='latin')

#%% Duplicando a variável 'dataNascimentoFull' a fim de juntar uma das colunas com o 'nomecompletoPaciente' para diminuir a incidência de homônimos
dupli_nasc = dados[['Record_Id','nomecompletoPaciente', 'dataNascimentoFull', 'dataNascimentoFull', 'sexo', 'dataPrimeiroSintomas', 'testeRT_PCR', 'Reinfecção']]
dados['nome_e_datanasc'] = dados['nomecompletoPaciente'] + ' ' + dados['dataNascimentoFull']

#%% Transformando algumas variáveis em letras minúsculas para padronização
dados['nomecompletoPaciente'] = dados['nomecompletoPaciente'].str.lower()
dados['sexo'] = dados['sexo'].str.lower()
dados['testeRT_PCR'] = dados['testeRT_PCR'].str.lower()
dados['Reinfecção'] = dados['Reinfecção'].str.lower()
dados['nome_e_datanasc'] = dados['nome_e_datanasc'].str.lower()

#%% Substituindo os acentos dos nomes dos pacientes para as letras sem acento
dados.replace({'nome_e_datanasc': {'à': 'a', 'á': 'a', 'â': 'a', 'ã': 'a',
            'è': 'e','é': 'e', 'ê': 'e',
            'í': 'i',
            'ò': 'o', 'ó': 'o', 'ô': 'o', 'õ': 'o',
            'ú': 'u', 'ü': 'u',  
            'ç': 'c'}}, regex=True, inplace=True)


#%% Substituindo os acentos da variável de reinfecção para as letras sem acento
dados.replace({'Reinfecção': {'à': 'a', 'á': 'a', 'â': 'a', 'ã': 'a'}}, regex=True, inplace=True)

#%% Removendo status de 'provavel', 'sim' e 'confirmada' da variável Reinfecção
dados.drop(dados[dados.Reinfecção == 'provavel'].index, inplace=True)
dados.drop(dados[dados.Reinfecção == 'sim'].index, inplace=True)
dados.drop(dados[dados.Reinfecção == 'confirmada'].index, inplace=True)

#%%  Calculando as idades das pessoas para definir faixas etárias para cada uma, a fim de fazer separação do banco para otimizar o processamento
# A referência para calcular a idade é sempre o dia que o código foi rodado 
def age(nasc):
    nasc = datetime.strptime(nasc, "%d/%m/%Y").date()
    hoje = date.today() 
    return hoje.year - nasc.year - ((hoje.month, 
                                      hoje.day) < (nasc.month, 
                                                    nasc.day))

#%% Atribuindo as idades às pessoas do banco de dados
dados['idade'] = dados['dataNascimentoFull'].apply(age)

#%% Definindo a faixa etária em que a pessoa se encaixa
faixa_etaria = []
for row in dados['idade']:
        if 0.0 <= row <= 20.0:  faixa_etaria.append('entre 0 e 20') 
        elif 21.0 <= row <= 30.0:  faixa_etaria.append('entre 21 e 30')
        elif 31.0 <= row <= 40.0:  faixa_etaria.append('entre 31 e 40')
        elif 41.0 <= row <= 50.0:  faixa_etaria.append('entre 41 e 50')
        elif 51.0 <= row <= 60.0:  faixa_etaria.append('entre 51 e 60')
        elif 61.0 <= row: faixa_etaria.append('acima de 61')
        else:           faixa_etaria.append('sem_idade')


#%% Adicionando a faixa etária ao banco de dados
dados['faixas_etarias'] = faixa_etaria

#%% Separando por sexo feminino e masculino para otimizar o processamento
dados_feminino = dados[dados['sexo']=='feminino']
dados_masculino = dados[dados['sexo']=='masculino']

#%% Criando um vetor de alfabetos, pois a ideia foi fazer o fuzzy match separado por sexo, letra do alfabeto e as faixas etárias
# para que o processamento fique mais rápido, ex: primeiro roda a faixa etária 'entre 0 e 20' da letra 'A' do sexo feminino, etc
alfabeto = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']
faixasE = ['entre 0 e 20', 'entre 21 e 30','entre 31 e 40',
             'entre 41 e 50','entre 51 e 60','acima de 61']

#%% Esta é a parte do código que separa o banco de dados nas partes espicificadas no código acima e que busca por duplicidades
# ESTE FAZ O FUZZY MATCH SOMENTE PARA O SEXO FEMININO E ESTE É O PROCESSO QUE LEVA MAIS TEMPO
duplicidade = []
score = []
for letra in alfabeto:
    dados_f2 = dados_feminino[dados_feminino['nome_e_datanasc'].str[0] == letra]
    for faixa in faixasE:
        dados_f = dados_f2[dados_f2['faixas_etarias'] == faixa]
        nomesF = dados_f['nome_e_datanasc']
        nomes_tratados = [utils.default_process(org) for org in nomesF] 
        for (k, duplicidades) in enumerate(nomes_tratados):
            nomes_tratados[k] = None
            match = process.extractOne(duplicidades, nomes_tratados, processor=None, score_cutoff=93)
            nomes_tratados[k] = duplicidades
            if match:
                duplicidade.append(list(nomesF)[match[2]])
                score.append(match[1])

#%% Gerando um dataframe com as duplicidades e scores encontrados
duplos = pd.DataFrame(list(zip(duplicidade, score)), columns =['Duplicidades', 'Score'])

#%% Limpando os nomes que estão no banco feminino e de duplos para juntar o banco 'duplos' 
# com o 'dados_feminino' e obter colunas com informações importantes que estão no banco 'dados_feminino'
nomesF2 = dados_feminino['nome_e_datanasc']
nomes_tratados = [utils.default_process(org) for org in nomesF2] #esta parte do código limpa os nomes tirando caracteres como '/', '-' e outros que possam ter sido digitados erroneamemte
dados_feminino['nomestratados'] = nomes_tratados
nomesFinal = duplos['Duplicidades']
nomes_tratados = [utils.default_process(org) for org in nomesFinal]
duplos['nomestratados'] = nomes_tratados

#%% Juntando os bancos; removendo as linhas que são NaN e manter somente as duplicidades encontradas;
# mudando o index do banco para 'Record_Id'; tirando variáveis que não são interessantes manter na planilha final
juntos = dados_feminino.set_index('nomestratados').join(duplos.set_index('nomestratados'))
duplicidadesFinal = juntos.dropna(subset=['Duplicidades'])
duplicidadesFinal = duplicidadesFinal.set_index('Record_Id')
duplicidadesFinal = duplicidadesFinal.drop(['sexo', 'nome_e_datanasc', 'faixas_etarias'], 1)

#%% Salvando o banco final em um aquivo CSV
duplicidadesFinal.to_csv('DuplicidadesPainel_FEMININO.csv',sep=';', encoding='latin', decimal=',')

#%% ***************DUPLICADOS DO SEXO MASCULINO***************
#%% Esta é a parte do código que separa o banco de dados nas partes espicificadas no código acima e que busca por duplicidades
# ESTE FAZ O FUZZY MATCH SOMENTE PARA O SEXO MASCULINO E ESTE É O PROCESSO QUE LEVA MAIS TEMPO
duplicidade = []
score = []
for letra in alfabeto:
    dados_m2 = dados_masculino[dados_masculino['nome_e_datanasc'].str[0] == letra]
    for faixa in faixasE:
        dados_m = dados_m2[dados_m2['faixas_etarias'] == faixa]
        nomesM = dados_m['nome_e_datanasc']
        nomes_tratados = [utils.default_process(org) for org in nomesM] 
        for (k, duplicidades) in enumerate(nomes_tratados):
            nomes_tratados[k] = None
            match = process.extractOne(duplicidades, nomes_tratados, processor=None, score_cutoff=93)
            nomes_tratados[k] = duplicidades
            if match:
                duplicidade.append(list(nomesM)[match[2]])
                score.append(match[1])

#%% Gerando um dataframe com as duplicidades e scores encontrados
duplosM = pd.DataFrame(list(zip(duplicidade, score)), columns =['Duplicidades', 'Score'])

#%% Limpando os nomes que estão no banco feminino e de duplos para juntar o banco 'duplos' 
# com o 'dados_feminino' e obter colunas com informações importantes que estão no banco 'dados_feminino'
nomesM2 = dados_masculino['nome_e_datanasc']
nomes_tratados = [utils.default_process(org) for org in nomesM2] #esta parte do código limpa os nomes tirando caracteres como '/', '-' e outros que possam ter sido digitados erroneamemte
dados_masculino['nomestratados'] = nomes_tratados
nomesFinal = duplosM['Duplicidades']
nomes_tratados = [utils.default_process(org) for org in nomesFinal]
duplosM['nomestratados'] = nomes_tratados

#%% Juntando os bancos; removendo as linhas que são NaN e manter somente as duplicidades encontradas;
# mudando o index do banco para 'Record_Id'; tirando variáveis que não são interessantes manter na planilha final
juntos = dados_masculino.set_index('nomestratados').join(duplosM.set_index('nomestratados'))
duplicidadesFinal_M = juntos.dropna(subset=['Duplicidades'])
duplicidadesFinal_M = duplicidadesFinal_M.set_index('Record_Id')
duplicidadesFinal_M = duplicidadesFinal_M.drop(['sexo', 'nome_e_datanasc', 'faixas_etarias'], 1)

#%% Salvando o banco final em um aquivo CSV
duplicidadesFinal_M.to_csv('DuplicidadesPainel_MASCULINO.csv',sep=';', encoding='latin', decimal=',')

