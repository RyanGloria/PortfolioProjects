-- Looking at data
SELECT *
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
ORDER BY location, date

--SELECT *
--FROM PortfolioProject..CovidVaccinations
--ORDER BY location, date

--Selecting data to use
SELECT location, date, total_cases, new_cases, total_deaths, population
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
ORDER BY location, date

-- Total Cases vs Total Deaths
SELECT location, date, total_cases, total_deaths, ROUND((total_deaths/total_cases)*100,3) AS DeathPercentage
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
ORDER BY location, date

-- Total Cases vs Population
SELECT location, date, total_cases, population, ROUND((total_cases/population)*100,3) AS CasePercentage
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
ORDER BY location, date

-- Countries with highest infection rate
SELECT location, population, MAX(total_cases) AS HighestInfectionCount, MAX(ROUND((total_cases/population)*100,3)) AS CasePercentage
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
GROUP BY location, population
ORDER BY CasePercentage DESC

-- Countries with highest infection rate (with dates)
SELECT location, population, date, MAX(total_cases) AS HighestInfectionCount, MAX(ROUND((total_cases/population)*100,3)) AS CasePercentage
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
GROUP BY location, population, date
ORDER BY CasePercentage DESC

-- Countries with highest death count
SELECT location, MAX(CAST(total_deaths AS INT)) AS DeathCount
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
GROUP BY location
ORDER BY DeathCount DESC


-- Looking at continents below


-- Continents with highest death count
SELECT location, SUM(CAST(new_deaths AS INT)) AS DeathCount
FROM PortfolioProject..CovidDeaths
WHERE continent is null
and location not in ('World','European Union','International','Upper middle income','High income','Lower middle income','Low income')
GROUP BY location
ORDER BY DeathCount DESC

-- Looking at global death percentage
SELECT SUM(new_cases) AS TotalCases, SUM(CAST(new_deaths AS INT)) AS TotalDeaths, (SUM(CAST(new_deaths AS INT))/SUM(new_cases))*100 AS DeathPercentage
FROM PortfolioProject..CovidDeaths
WHERE continent is not null
--GROUP BY date
ORDER BY 1,2


-- Total population vs vaccinations
--CTE
with PopVsVac (continent, location, date, population, new_vaccinations, RollingVacCount) AS (
SELECT d.continent, d.location, d.date, d.population, v.new_vaccinations
, SUM(CAST(v.new_vaccinations AS bigint)) OVER (PARTITION BY d.location ORDER BY d.location, d.date) AS RollingVacCount
FROM PortfolioProject..CovidDeaths d
JOIN PortfolioProject..CovidVaccinations v
ON d.location = v.location
AND d.date = v.date
WHERE d.continent is not null
--ORDER BY 2,3
)
SELECT *, (RollingVacCount/population)*100
FROM PopVsVac


-- Total population vs vaccinations
-- Temp Table
DROP TABLE if exists #PercentPopulationVaccinated
CREATE TABLE #PercentPopulationVaccinated
(
continent nvarchar(255),
location nvarchar(255),
date datetime,
population numeric,
new_vaccinations numeric,
RollingVacCount numeric
)
INSERT INTO #PercentPopulationVaccinated
SELECT d.continent, d.location, d.date, d.population, v.new_vaccinations
, SUM(CAST(v.new_vaccinations AS bigint)) OVER (PARTITION BY d.location ORDER BY d.location, d.date) AS RollingVacCount
FROM PortfolioProject..CovidDeaths d
JOIN PortfolioProject..CovidVaccinations v
ON d.location = v.location
AND d.date = v.date
WHERE d.continent is not null
ORDER BY 2,3

SELECT *, (RollingVacCount/population)*100
FROM #PercentPopulationVaccinated


-- Total population vs vaccinations
-- Creating View
Create View PercentPopulationVaccinated AS 
SELECT d.continent, d.location, d.date, d.population, v.new_vaccinations
, SUM(CAST(v.new_vaccinations AS bigint)) OVER (PARTITION BY d.location ORDER BY d.location, d.date) AS RollingVacCount
FROM PortfolioProject..CovidDeaths d
JOIN PortfolioProject..CovidVaccinations v
ON d.location = v.location
AND d.date = v.date
WHERE d.continent is not null
--ORDER BY 2,3

SELECT *, (RollingVacCount/population)*100
FROM PercentPopulationVaccinated
